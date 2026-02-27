unit bypass;

interface

uses SysUtils, Classes;

type
  TNPCManager = class
  public
    // Variáveis para memorizar onde o boneco estava ANTES do clique
    FMemX, FMemY, FMemZ: Integer;
    
    constructor Create;
    destructor Destroy; override;

    function ClicarBypass(Indice: Integer): Boolean;
    procedure AguardarTeleporte(TimeoutSegundos: Integer);

    function Interagir(NomeNPC: string; const Bypasses: array of Integer): Boolean;
   
    // Na interface, adicionar:
    function ClicarBypassPorNome(NomeOpcao: string): Boolean;
    function InteragirPorNome(NomeNPC: string; const Opcoes: array of string): Boolean;



    procedure setMemoriaPosicaoAtual; // Método para atualizar a memória com a posição atual do personagem
    procedure QuestRosella;
    procedure QuestGreenis;
    procedure QuestMirabel;
    procedure QuestThalia;
  end;

var
  NPCManager: TNPCManager;

implementation

constructor TNPCManager.Create;
begin
  inherited Create; 
  FMemX := 0; FMemY := 0; FMemZ := 0;
end;

destructor TNPCManager.Destroy;
begin
  inherited Destroy;
end;

procedure TNPCManager.setMemoriaPosicaoAtual;
var
  LUser: TL2User;
begin
  LUser := Engine.GetUser;
  if LUser <> nil then begin
    FMemX := LUser.X;
    FMemY := LUser.Y;
    FMemZ := LUser.Z;
  end;
end;

function TNPCManager.ClicarBypass(Indice: Integer): Boolean;
var
  Html, Comando: string;
  i, PosBypass, PosFim: Integer;
begin
  Result := False;
  Html := Engine.DlgText;
  if (Html = '') or (Indice <= 0) then Exit;

  for i := 1 to Indice do
  begin
    PosBypass := Pos('bypass -h ', Html);
    if PosBypass > 0 then
    begin
      Delete(Html, 1, PosBypass + 9);
      if i = Indice then
      begin
        PosFim := Pos('"', Html);
        if PosFim > 0 then
        begin
          Comando := Copy(Html, 1, PosFim - 1);
          Print('>>> [Bypass] Enviando: ' + Comando);
          Engine.BypassToServer(Comando);
          Result := True;
          Exit;
        end;
      end;
    end else Break;
  end;
end;

procedure TNPCManager.AguardarTeleporte(TimeoutSegundos: Integer);
var
  TempoInicial: Cardinal;
  Teleportou: Boolean;
  LUser: TL2User;
  Dist: Double;
  DeltaZ: Integer;
begin
  Teleportou := False;
  TempoInicial := GetTickCount;

  // Se a memória estiver vazia, captura posição atual
  if (FMemX = 0) and (FMemY = 0) then begin
    LUser := Engine.GetUser;
    if LUser <> nil then begin
      FMemX := LUser.X; FMemY := LUser.Y; FMemZ := LUser.Z;
    end;
  end;

  Print('>>> [Teleporte] Checando a partir de X:' + IntToStr(FMemX) +
        ' Y:' + IntToStr(FMemY) + ' Z:' + IntToStr(FMemZ));

  while (GetTickCount - TempoInicial < Cardinal(TimeoutSegundos * 1000)) do
  begin
    LUser := Engine.GetUser;

    // Caso 1: Loading / Offline
    if (Engine.Status <> lsOnline) or (LUser = nil) then
    begin
      Print('>>> [Teleporte] Caso 1: Loading detectado.');
      Teleportou := True;
      while (Engine.Status <> lsOnline) and
            (GetTickCount - TempoInicial < Cardinal(TimeoutSegundos * 1000)) do
        Delay(500);
      Break;
    end;

    // Caso 2: Teleporte horizontal normal (XY diverge muito)
    Dist := Sqrt(Sqr(Double(LUser.X - FMemX)) + Sqr(Double(LUser.Y - FMemY)));
    if (Dist > 2000) then
    begin
      Print('>>> [Teleporte] Caso 2: Distância XY divergiu. Dist2D: ' + FormatFloat('0', Dist));
      Teleportou := True;
      Break;
    end;

    // Caso 3: Teleporte vertical (ex: Ivory Tower, Tower of Insolence)
    // X/Y quase iguais mas Z muda drasticamente
    DeltaZ := Abs(LUser.Z - FMemZ);
    if (DeltaZ > 400) then
    begin
      Print('>>> [Teleporte] Caso 3: Delta Z elevado (torre/andar). DeltaZ: ' + IntToStr(DeltaZ));
      Teleportou := True;
      Break;
    end;

    Delay(200);
  end;

  // Limpa memória para o próximo uso
  FMemX := 0; FMemY := 0; FMemZ := 0;

  if Teleportou then
  begin
    Print('>>> [Teleporte] Finalizado com sucesso.');
    Delay(2000);
  end else
    Print('>>> [Teleporte] Erro: Timeout.');
end;


function TNPCManager.Interagir(NomeNPC: string; const Bypasses: array of Integer): Boolean;
var
  NPC: TL2Npc;
  LUser: TL2User;
  Tentativas, i: Integer;
  S: string;
begin
  Result := False; // padrão: falhou

  if not Engine.GetNpcList.ByName(NomeNPC, NPC) then
  begin
    Print('>>> [Erro] NPC nao encontrado: ' + NomeNPC);
    Exit;
  end;

  LUser := Engine.GetUser;
  if (LUser = nil) then Exit;

  FMemX := LUser.X;
  FMemY := LUser.Y;
  FMemZ := LUser.Z;

  if LUser.DistTo(NPC) > 150 then
  begin
    Engine.MoveTo(NPC.X, NPC.Y, NPC.Z);
    while (LUser.Moved) and (LUser.DistTo(NPC) > 120) do Delay(100);
  end;

  Engine.SetTarget(NPC);
  Delay(500);

  if not Engine.DlgOpen() then
  begin
    Engine.UseAction(0);
    Delay(800);
  end;

  for i := 0 to High(Bypasses) do
  begin
    if (Bypasses[i] <= 0) then Continue;

    if Engine.DlgText = '' then
    begin
      Print('>>> [Bypass] Janela fechou antes do passo ' + IntToStr(i + 1));
      Exit; // Result ainda é False
    end;

    S := Engine.DlgText;
    if ClicarBypass(Bypasses[i]) then
    begin
      Tentativas := 0;
      repeat
        Delay(450);
        Inc(Tentativas);
      until (Engine.DlgText <> S) or (Engine.DlgText = '') or (Tentativas >= 8);
    end;

    Delay(500);
  end;

  Result := True; // só chega aqui se passou por todos os bypasses sem erro
end;

function TNPCManager.ClicarBypassPorNome(NomeOpcao: string): Boolean;
var
  Html, Trecho, TextoBotao: string;
  Indice, PosBypass, PosAbreTag, PosFechaTag: Integer;
begin
  Result := False;
  Html := Engine.DlgText;
  if (Html = '') or (NomeOpcao = '') then Exit;

  Indice := 0;
  Trecho := Html;

  repeat
    // Acha o próximo "bypass -h "
    PosBypass := Pos('bypass -h ', Trecho);
    if PosBypass = 0 then Break;

    Inc(Indice);
    Delete(Trecho, 1, PosBypass + 9); // avança para depois de "bypass -h "

    // Agora pula até o ">" que fecha a tag <a action="...">
    PosAbreTag := Pos('>', Trecho);
    if PosAbreTag = 0 then Break;

    // Pega o texto do botão entre ">" e "</a>"
    Delete(Trecho, 1, PosAbreTag);
    PosFechaTag := Pos('<', Trecho);
    if PosFechaTag = 0 then Break;

    TextoBotao := Copy(Trecho, 1, PosFechaTag - 1);

    Print('>>> [Debug] Bypass #' + IntToStr(Indice) + ' = "' + TextoBotao + '"');

    // Compara com o nome buscado
    if Pos(LowerCase(Trim(NomeOpcao)), LowerCase(TextoBotao)) > 0 then
    begin
      Result := ClicarBypass(Indice); // reutiliza o que já funciona
      Exit;
    end;

  until False;

  Print('>>> [Bypass] Opcao nao encontrada: "' + NomeOpcao + '"');
end;

function TNPCManager.InteragirPorNome(NomeNPC: string; const Opcoes: array of string): Boolean;
var
  NPC: TL2Npc;
  LUser: TL2User;
  Tentativas, i: Integer;
  S: string;
  TempoInicial: Cardinal;
begin
  Result := False;

  // Aguarda NPC aparecer na lista por até 10s
  TempoInicial := GetTickCount;
  while not Engine.GetNpcList.ByName(NomeNPC, NPC) do
  begin
    if (GetTickCount - TempoInicial > 10000) then
    begin
      Print('>>> [Erro] NPC nao encontrado apos 10s: ' + NomeNPC);
      Exit;
    end;
    Print('>>> [Aguardando] NPC nao visivel ainda: ' + NomeNPC);
    Delay(1000);
  end;

  LUser := Engine.GetUser;
  if (LUser = nil) then Exit;

  FMemX := LUser.X;
  FMemY := LUser.Y;
  FMemZ := LUser.Z;

  if LUser.DistTo(NPC) > 150 then
  begin
    Engine.MoveTo(NPC.X, NPC.Y, NPC.Z);
    while (LUser.Moved) and (LUser.DistTo(NPC) > 120) do Delay(100);
  end;

  Engine.SetTarget(NPC);
  Delay(500);

  if not Engine.DlgOpen() then
  begin
    Engine.UseAction(0);
    Delay(800);
  end;

  for i := 0 to High(Opcoes) do
  begin
    if Opcoes[i] = '' then Continue;

    if Engine.DlgText = '' then
    begin
      Print('>>> [Bypass] Janela fechou antes do passo ' + IntToStr(i + 1));
      Exit;
    end;

    S := Engine.DlgText;
    if ClicarBypassPorNome(Opcoes[i]) then
    begin
      Tentativas := 0;
      repeat
        Delay(450);
        Inc(Tentativas);
      until (Engine.DlgText <> S) or (Engine.DlgText = '') or (Tentativas >= 8);
    end;

    Delay(500);
  end;

  Result := True;
end;



// Quests - sintaxe com colchetes []
procedure TNPCManager.QuestRosella;  begin Interagir('Rosella',        [1, 1]);    end;
procedure TNPCManager.QuestGreenis;  begin Interagir('Greenis',        [2, 1]);    end;
procedure TNPCManager.QuestMirabel;  begin Interagir('Teleport Device',[1, 5]);    end;
procedure TNPCManager.QuestThalia;   begin Interagir('Thalia',         [2, 1]);    end;

initialization
  NPCManager := TNPCManager.Create;

finalization
  if Assigned(NPCManager) then
    FreeAndNil(NPCManager);

end.
