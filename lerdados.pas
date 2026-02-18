unit lerdados;

interface

uses SysUtils, Classes;

var SKILL_ID: Integer   = 1177;//(Window Wind) ID da skill a ser usada no combate (defina como -1 para usar ataque básico)

procedure SeguirCaminhoGravado(NomeArquivo: string);

implementation

type
  TPonto = record
    X, Y, Z : Integer;
    ID_Rota  : Integer;
    EhParada : Boolean;
    Tag      : string;
  end;
  TPontoArray = array of TPonto;

const
  RAIO_CHEGADA          = 45;
  RAIO_PASSAGEM         = 150;
  DIST_Z_LIMITE         = 250;
  TIMEOUT_MS            = 12000;
  DESVIO_MAX            = 50;
  MAX_TENTATIVAS        = 5;
  TIMEOUT_PROGRESSO_MS  = 5000;
  DELTA_PROGRESSO       = 30;
 

var
  GEmCombate: Boolean = False; // Variável global para sinalizar que estamos em combate, setada pelos Handlers de movimento e verificada na lógica de movimentação para esperar o combate terminar antes de continuar a movimentação.

// --- HANDLERS DE EVENTOS ---
procedure OnMoveEvent(Attacker: TL2Live; var StopMove: boolean);
begin
  if (Attacker <> nil) and Attacker.valid and (Attacker.L2Class = lcNpc) then begin
    StopMove   := True;
    GEmCombate := True;
  end;
end;

procedure OnFree;
begin
  GEmCombate := False;
end;

// --- LEITURA E UTILITÁRIOS ---
function LocalizarArquivo(const Nome: string): string;
begin
  Result := ExePath + 'caminhos\' + Nome;
  if not FileExists(Result) then Result := ExePath + Nome;
end;

function ExtrairCoord(const Linha, Eixo: string): Integer;
var p, i: Integer; s: string;
begin
  Result := 0; s := '';
  p := Pos(UpperCase(Eixo) + ':', UpperCase(Linha));
  if p = 0 then Exit;
  i := p + Length(Eixo) + 1;
  while (i <= Length(Linha)) and (Linha[i] = ' ') do Inc(i);
  while (i <= Length(Linha)) and (Linha[i] in ['-', '0'..'9']) do begin
    s := s + Linha[i]; Inc(i);
  end;
  if (s <> '') and (s <> '-') then Result := StrToInt(s);
end;

function CarregarPontos(const NomeArquivo: string; out Pontos: TPontoArray): Integer;
var Lista: TStringList; i, ID_Atual: Integer; Linha: string;
begin
  Result := 0; ID_Atual := 0;
  if not FileExists(NomeArquivo) then Exit;
  Lista := TStringList.Create;
  try
    Lista.LoadFromFile(NomeArquivo);
    SetLength(Pontos, Lista.Count);
    for i := 0 to Lista.Count - 1 do begin
      Linha := UpperCase(Lista[i]);
      if Linha = '' then Continue;
      if Pos('INICIO', Linha) > 0 then ID_Atual := ID_Atual + 1;
      if Pos('X:', Linha) = 0 then Continue;
      Pontos[Result].X := ExtrairCoord(Linha, 'X');
      Pontos[Result].Y := ExtrairCoord(Linha, 'Y');
      Pontos[Result].Z := ExtrairCoord(Linha, 'Z');
      Pontos[Result].ID_Rota := ID_Atual;
      Pontos[Result].EhParada := (Pos('PARADA', Linha) > 0);
      Inc(Result);
    end;
    SetLength(Pontos, Result);
  finally Lista.Free; end;
end;

procedure LimparTargetInvalido;
begin
  if (User.Target.valid) then
  begin
    // Checagem tripla para garantir que o corpo seja limpo corretamente, evitando bugs de "target morto" que impedem o movimento
    if User.Target.Dead or (User.Target.HP <= 0) then
    begin
      Engine.CancelTarget;
      delay(100); // Delay levemente maior para sincronia do servidor
    end;
  end;
end;

function MatarMobAgressor: Boolean;
var
  Inimigo: TL2Live;
  Falhas: Integer;
  UsarSkill: Boolean;
begin
  Result := False;
  if (User = nil) or (not User.valid) or User.Dead then Exit;

  UsarSkill := (SKILL_ID >= 0);

  while Engine.FindEnemy(Inimigo, User, 1000, 300) do
  begin
    if (Inimigo = nil) or (not Inimigo.valid) or Inimigo.Dead then Break;

    Result := True;
    Print('>>> GPS: [COMBATE] Focando em ' + Inimigo.Name);
    Engine.SetTarget(Inimigo);
    Falhas := 0;

    while Inimigo.valid and (not Inimigo.Dead) and (Inimigo.HP > 0) do
    begin
      if (User = nil) or User.Dead or (not User.valid) then Exit;

      if UsarSkill then
      begin
        // Modo Skill [page:0]
        if Engine.UseSkill(SKILL_ID) then
        begin
          Falhas := 0;
          while (User.Cast.EndTime > 0) do
          begin
            delay(30);
            if (not Inimigo.valid) or Inimigo.Dead or (Inimigo.HP <= 0) then Break;
          end;
          delay(50);
        end
        else
        begin
          Inc(Falhas);
          delay(200);
          if Falhas >= 10 then
          begin
            Print('>>> GPS: [AVISO] Falha ao atingir alvo (skill), trocando...');
            Break;
          end;
        end;
      end
      else
      begin
        // Modo Ataque Básico: Engine.Attack [page:0]
        if Engine.Attack then
        begin
          Falhas := 0;
          // Ataque básico não tem "cast", espera um tick de ataque
          // User.AttackEndTime se existir na sua versão; senão, delay fixo
          delay(400);
        end
        else
        begin
          Inc(Falhas);
          delay(200);
          if Falhas >= 10 then
          begin
            Print('>>> GPS: [AVISO] Falha ao atingir alvo (ataque), trocando...');
            Break;
          end;
        end;
      end;
    end;

    Print('>>> GPS: [COMBATE] Alvo eliminado.');
    Engine.CancelTarget;
    delay(200);
  end;
end;


procedure AguardarFimDeCombate;
begin
  GEmCombate := True;
  while True do begin
    if not MatarMobAgressor then begin
      GEmCombate := False;
      if (User <> nil) and User.Target.valid then Engine.CancelTarget;
      Break;
    end;
    delay(100);
    if (User = nil) or (not User.valid) or User.Dead then Break;
  end;
end;

// --- LÓGICA DE MOVIMENTAÇÃO ---
function AcharProximoAlvoLimitado(const Rota: TPontoArray; Atual, Limite: Integer): Integer;
var j, k: Integer; Lx, Ly, Kx, Ky, Cruz, L2, MaxCruz: Int64;
begin
  Result := Atual;
  for j := Atual + 1 to Limite do begin
    Lx := Int64(Rota[j].X) - Rota[Atual].X; Ly := Int64(Rota[j].Y) - Rota[Atual].Y;
    L2 := Lx * Lx + Ly * Ly; MaxCruz := 0;
    for k := Atual + 1 to j - 1 do begin
      Kx := Int64(Rota[k].X) - Rota[Atual].X; Ky := Int64(Rota[k].Y) - Rota[Atual].Y;
      Cruz := Abs(Lx * Ky - Ly * Kx);
      if Cruz > MaxCruz then MaxCruz := Cruz;
    end;
    if (L2 > 0) and (MaxCruz * MaxCruz > Int64(DESVIO_MAX) * Int64(DESVIO_MAX) * L2) then Break;
    Result := j;
  end;
end;

function ExecutarBloco(const Rota: TPontoArray; InicioIdx: Integer): Boolean;
var
  i, Alvo, SegInicio, SegFim: Integer;
  Dist, UltDist: Double;
  TickInicio, TickSemProgresso: Cardinal;
  InimigoFake: TL2Live; // Variável apenas para o teste do FindEnemy
begin
  Result := False; SegInicio := InicioIdx;
  while SegInicio <= High(Rota) do begin
    SegFim := High(Rota);
    for i := SegInicio to High(Rota) do if Rota[i].EhParada then begin SegFim := i; Break; end;
    
    i := SegInicio;
    while i <= SegFim do begin
      if (User = nil) or (not User.valid) then Exit;

      // Ignoramos User.InCombat para evtar esperar o tempo de combate. Verificamos se há ameaça real ou se o Handler setou combate.
      if GEmCombate or Engine.FindEnemy(InimigoFake, User, 1000, 300) then AguardarFimDeCombate;

      Alvo := AcharProximoAlvoLimitado(Rota, i, SegFim);
      Engine.DMoveTo(Rota[Alvo].X, Rota[Alvo].Y, Rota[Alvo].Z);
      
      TickInicio := GetTickCount; TickSemProgresso := GetTickCount;
      UltDist := User.DistTo(Rota[Alvo].X, Rota[Alvo].Y, Rota[Alvo].Z);
      
      while True do begin
        delay(50);
        if (User = nil) or (not User.valid) or User.Dead then Exit;

        //  Mesma lógica dentro do loop de movimento pra evitar usar o .InCombat, que pode ser impreciso. Se o Handler de movimento setou que estamos em combate, ou se há um inimigo próximo, consideramos que estamos em combate e esperamos ele terminar antes de continuar a movimentação.
        if GEmCombate or Engine.FindEnemy(InimigoFake, User, 1000, 300) then begin
          Engine.CancelTarget; 
          AguardarFimDeCombate;
          Engine.DMoveTo(Rota[Alvo].X, Rota[Alvo].Y, Rota[Alvo].Z);
          TickInicio := GetTickCount; TickSemProgresso := GetTickCount; Continue;
        end;

        Dist := User.DistTo(Rota[Alvo].X, Rota[Alvo].Y, Rota[Alvo].Z);
        if (UltDist - Dist) > DELTA_PROGRESSO then begin UltDist := Dist; TickSemProgresso := GetTickCount; end;
        if (GetTickCount - TickSemProgresso) > TIMEOUT_PROGRESSO_MS then Exit;
        
        if (Alvo = SegFim) then begin if Dist < RAIO_CHEGADA then Break; end
        else begin if Dist < RAIO_PASSAGEM then Break; end;
        
        if not User.Moved then Engine.DMoveTo(Rota[Alvo].X, Rota[Alvo].Y, Rota[Alvo].Z);
        if (GetTickCount - TickInicio) > TIMEOUT_MS then Break;
      end;
      i := Alvo + 1;
    end;
    if SegFim = High(Rota) then begin Result := True; Exit; end;
    SegInicio := SegFim + 1;
  end;
end;

// --- LÓGICA DE IDENTIFICAÇÃO DE BLOCO ---
procedure SeguirCaminhoGravado(NomeArquivo: string);
var Tudo, Rota: TPontoArray; i, IndiceGlobal, ID_Alvo, Cont, Tentativa, InicioRetomada: Integer; MenorDist, dTmp: Double; Chegou: Boolean;
begin
  Cont := CarregarPontos(LocalizarArquivo(NomeArquivo), Tudo);
  if (Cont = 0) or (User = nil) or (not User.valid) then Exit;

  // BUSCA GLOBAL: Encontra o ponto mais próximo em qualquer um dos blocos (IDs)
  IndiceGlobal := -1;
  MenorDist    := 999999;
  for i := 0 to High(Tudo) do begin
    if Abs(User.Z - Tudo[i].Z) > DIST_Z_LIMITE then Continue;
    dTmp := User.DistTo(Tudo[i].X, Tudo[i].Y, Tudo[i].Z);
    if dTmp < MenorDist then begin
      MenorDist    := dTmp;
      IndiceGlobal := i;
    end;
  end;

  if IndiceGlobal < 0 then begin
    Print('>>> GPS: [ERRO] Nenhum ponto proximo encontrado no arquivo.');
    Exit;
  end;

  // Trava no ID da rota onde o ponto mais próximo foi encontrado
  ID_Alvo := Tudo[IndiceGlobal].ID_Rota;
  SetLength(Rota, Cont);
  Cont := 0;
  InicioRetomada := -1;
  for i := 0 to High(Tudo) do begin
    if Tudo[i].ID_Rota = ID_Alvo then begin
      Rota[Cont] := Tudo[i];
      if i = IndiceGlobal then InicioRetomada := Cont;
      Inc(Cont);
    end;
  end;
  SetLength(Rota, Cont);

  Print('>>> GPS: Iniciando Bloco ' + IntToStr(ID_Alvo) + ' a partir do ponto ' + IntToStr(InicioRetomada+1));

  Tentativa := 0; Chegou := False;
  while (not Chegou) and (Tentativa < MAX_TENTATIVAS) do begin
    if (User = nil) or (not User.valid) then Break;
    if Tentativa > 0 then begin
      delay(1000);
      MenorDist := 999999;
      for i := 0 to High(Rota) do begin
        dTmp := User.DistTo(Rota[i].X, Rota[i].Y, Rota[i].Z);
        if dTmp < MenorDist then begin MenorDist := dTmp; InicioRetomada := i; end;
      end;
    end;
    Chegou := ExecutarBloco(Rota, InicioRetomada);
    Inc(Tentativa);
  end;

  if Chegou then Print('>>> GPS: [OK] Percurso concluido.') else Print('>>> GPS: [FALHA] Abortando.');
end;

end.