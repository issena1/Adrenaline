unit lerdados;

interface

uses SysUtils, Classes;

var SKILL_ID: Integer = 1177;

procedure SeguirCaminhoGravado(NomeArquivo: string);

implementation

type
  TPonto = record
    X, Y, Z  : Integer;
    EhParada : Boolean;
    ID_Bloco : Integer;
  end;
  TPontoArray = array of TPonto;

const
  RAIO_FLUIDO   = 130;
  RAIO_PARADA   = 40;
  DESVIO_MAX    = 40;
  DIST_Z_LIMITE = 300;
  TIMEOUT_STUCK = 5000;

// --- UTILITÁRIOS ---

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
  if p > 0 then begin
    i := p + Length(Eixo) + 1;
    while (i <= Length(Linha)) and (not (Linha[i] in ['-', '0'..'9'])) do Inc(i);
    while (i <= Length(Linha)) and (Linha[i] in ['-', '0'..'9']) do begin
      s := s + Linha[i]; Inc(i);
    end;
    if (s <> '') and (s <> '-') then Result := StrToInt(s);
  end;
end;

function CarregarPontos(const NomeArquivo: string; out Pontos: TPontoArray): Integer;
var Lista: TStringList; i, BlocoAtual: Integer; Linha: string;
begin
  Result := 0;
  BlocoAtual := 0;
  if not FileExists(NomeArquivo) then Exit;
  Lista := TStringList.Create;
  try
    Lista.LoadFromFile(NomeArquivo);
    SetLength(Pontos, Lista.Count);
    for i := 0 to Lista.Count - 1 do begin
      Linha := UpperCase(Lista[i]);
      if Linha = '' then Continue;

      // CORREÇÃO: checar INICIO antes de filtrar por X:
      // Suporta tanto "INICIO" sozinho quanto "INICIO X:... Y:... Z:..."
      if Pos('INICIO', Linha) > 0 then Inc(BlocoAtual);

      // Ignora linhas sem coordenadas (marcadores, comentários, etc.)
      if Pos('X:', Linha) = 0 then Continue;

      Pontos[Result].X        := ExtrairCoord(Linha, 'X');
      Pontos[Result].Y        := ExtrairCoord(Linha, 'Y');
      Pontos[Result].Z        := ExtrairCoord(Linha, 'Z');
      Pontos[Result].ID_Bloco := BlocoAtual;
      Pontos[Result].EhParada := (Pos('PARADA', Linha) > 0);
      Inc(Result);
    end;
    SetLength(Pontos, Result);
  finally Lista.Free; end;
end;

// --- COMBATE ---

procedure AguardarFimDeCombate;
var Inimigo: TL2Live;
begin
  while Engine.FindEnemy(Inimigo, User, 1000) do begin
    Engine.SetTarget(Inimigo);
    while Inimigo.valid and (not Inimigo.Dead) and (not User.Dead) do begin
      if SKILL_ID >= 0 then Engine.UseSkill(SKILL_ID) else Engine.Attack;
      Delay(300);
    end;
    Engine.CancelTarget;
    // sai do outer-while se morreu
    if User.Dead then Exit;
  end;
end;

// --- GEOMETRIA ---

function CalcularAlvoOtimizado(const Rota: TPontoArray; AtualIdx, FimIdx: Integer): Integer;
var j, k: Integer; Lx, Ly, Kx, Ky, L2, Cruz: Int64;
begin
  Result := AtualIdx;
  for j := AtualIdx + 1 to (AtualIdx + 7) do begin
    if j > FimIdx then Break;       // Não ultrapassa fim do bloco
    if Rota[j-1].EhParada then Break; // Para antes de PARADA intermediária

    Lx := Int64(Rota[j].X) - Rota[AtualIdx].X;
    Ly := Int64(Rota[j].Y) - Rota[AtualIdx].Y;
    L2 := Lx * Lx + Ly * Ly;
    if L2 = 0 then Continue;

    for k := AtualIdx + 1 to j - 1 do begin
      Kx   := Int64(Rota[k].X) - Rota[AtualIdx].X;
      Ky   := Int64(Rota[k].Y) - Rota[AtualIdx].Y;
      Cruz := Abs(Lx * Ky - Ly * Kx);
      if (Cruz * Cruz) > (Int64(DESVIO_MAX) * Int64(DESVIO_MAX) * L2) then Exit;
    end;
    Result := j;
  end;
end;

// --- LÓGICA PRINCIPAL ---

procedure SeguirCaminhoGravado(NomeArquivo: string);
var
  Tudo              : TPontoArray;
  i                 : Integer;
  IndiceAtual       : Integer;
  AlvoIdx           : Integer;
  UltimoAlvoEnviado : Integer;
  BlocoAtivo        : Integer;
  IndiceFimBloco    : Integer;
  Dist              : Double;
  MenorDist         : Double;
  TickStuck         : Cardinal;
  TickMove          : Cardinal;
  TickLog           : Cardinal;
  InimigoDet        : TL2Live;
  LastX, LastY      : Integer;
begin
  if CarregarPontos(LocalizarArquivo(NomeArquivo), Tudo) = 0 then begin
    Print('>>> GPS: Erro ao carregar arquivo ou arquivo vazio.');
    Exit;
  end;

  // 1. PONTO MAIS PRÓXIMO EM TODOS OS BLOCOS DO ARQUIVO
  IndiceAtual := -1;
  MenorDist   := 999999;
  for i := 0 to High(Tudo) do begin
    Dist := User.DistTo(Tudo[i].X, Tudo[i].Y, Tudo[i].Z);
    if (Dist < MenorDist) and (Abs(User.Z - Tudo[i].Z) < DIST_Z_LIMITE) then begin
      MenorDist   := Dist;
      IndiceAtual := i;
    end;
  end;

  if IndiceAtual = -1 then begin
    Print('>>> GPS: Personagem longe de qualquer rota.');
    Exit;
  end;

  // 2. IDENTIFICA E TRAVA NO BLOCO DO PONTO MAIS PRÓXIMO
  BlocoAtivo     := Tudo[IndiceAtual].ID_Bloco;
  IndiceFimBloco := IndiceAtual;

  // Varre para frente para encontrar o último ponto do bloco
  for i := IndiceAtual to High(Tudo) do begin
    if Tudo[i].ID_Bloco = BlocoAtivo then
      IndiceFimBloco := i
    else
      Break;
  end;

  Print('>>> GPS: Bloco ' + IntToStr(BlocoAtivo) +
        ' | Ponto inicial: ' + IntToStr(IndiceAtual) +
        ' | Ponto final: '   + IntToStr(IndiceFimBloco));

  // 3. INICIALIZAÇÃO (tudo antes do loop)
  LastX             := User.X;
  LastY             := User.Y;
  UltimoAlvoEnviado := -1;
  TickStuck         := GetTickCount;
  TickMove          := GetTickCount;
  TickLog           := GetTickCount;

  // 4. LOOP RESTRITO AO BLOCO ATIVO
  while (IndiceAtual <= IndiceFimBloco) and (not User.Dead) do begin

    // COMBATE
    if Engine.FindEnemy(InimigoDet, User, 800) then begin
      AguardarFimDeCombate;
      if User.Dead then Continue; // while-condition falha → sai limpo

      // Re-ancora dentro do bloco ativo
      MenorDist := 999999;
      for i := 0 to High(Tudo) do begin
        if Tudo[i].ID_Bloco <> BlocoAtivo then Continue;
        Dist := User.DistTo(Tudo[i].X, Tudo[i].Y, Tudo[i].Z);
        if (Dist < MenorDist) and (Abs(User.Z - Tudo[i].Z) < DIST_Z_LIMITE) then begin
          MenorDist   := Dist;
          IndiceAtual := i;
        end;
      end;
      UltimoAlvoEnviado := -1;
      TickStuck := GetTickCount;
      // ✅ CORREÇÃO: reseta posição de referência após combate
      LastX := User.X;
      LastY := User.Y;
    end;

    // CÁLCULO DO ALVO (limitado ao fim do bloco)
    AlvoIdx := CalcularAlvoOtimizado(Tudo, IndiceAtual, IndiceFimBloco);
    Dist    := User.DistTo(Tudo[AlvoIdx].X, Tudo[AlvoIdx].Y, Tudo[AlvoIdx].Z);

    // ENVIO DE MOVIMENTO
    if (AlvoIdx <> UltimoAlvoEnviado) or (not User.Moved)
    or (GetTickCount - TickMove > 5000) then begin
      Engine.DMoveTo(Tudo[AlvoIdx].X, Tudo[AlvoIdx].Y, Tudo[AlvoIdx].Z);
      UltimoAlvoEnviado := AlvoIdx;
      TickMove          := GetTickCount;
    end;

    // ANTI-STUCK (por posição real, não por distância)
    if (Abs(User.X - LastX) > 15) or (Abs(User.Y - LastY) > 15) then begin
      LastX     := User.X;
      LastY     := User.Y;
      TickStuck := GetTickCount;
    end else if (GetTickCount - TickStuck > TIMEOUT_STUCK) then begin
      Print('>>> GPS: Travado! Desviando...');
      Engine.DMoveTo(User.X + Random(200) - 100, User.Y + Random(200) - 100, User.Z);
      Delay(1000);
      TickStuck         := GetTickCount;
      UltimoAlvoEnviado := -1;
    end;

    // TRANSIÇÃO DE PONTOS
    if Tudo[AlvoIdx].EhParada then begin
      if Dist < RAIO_PARADA then IndiceAtual := AlvoIdx + 1;
    end else begin
      if Dist < RAIO_FLUIDO then IndiceAtual := AlvoIdx + 1;
    end;

    // LOG A CADA 1 SEGUNDO
    if GetTickCount - TickLog > 1000 then begin
      Print('>>> GPS: Bloco[' + IntToStr(BlocoAtivo) + '] ' +
            IntToStr(AlvoIdx) + '/' + IntToStr(IndiceFimBloco) +
            ' dist=' + FormatFloat('0.0', Dist));
      TickLog := GetTickCount;
    end;

    Delay(30);
  end;

  if not User.Dead then
    Print('>>> GPS: Fim do bloco ' + IntToStr(BlocoAtivo) + ' alcancado.')
  else
    Print('>>> GPS: Rota interrompida por morte.');
end;

end.
