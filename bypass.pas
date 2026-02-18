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
    procedure Interagir(NomeNPC: string; P1: Integer; P2: Integer; P3: Integer);
    
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
begin
  Teleportou := False;
  TempoInicial := GetTickCount;

  // Se por algum motivo a memória estiver vazia, captura a posição atual
  if (FMemX = 0) then begin
    LUser := Engine.GetUser;
    if LUser <> nil then begin
      FMemX := LUser.X; FMemY := LUser.Y; FMemZ := LUser.Z;
    end;
  end;

  Print('>>> [Teleporte] Checando movimento a partir de X:' + IntToStr(FMemX));

  while (GetTickCount - TempoInicial < (TimeoutSegundos * 1000)) do
  begin
    LUser := Engine.GetUser;

    // Caso 1: Loading / Offline
    if (Engine.Status <> lsOnline) or (LUser = nil) then
    begin
      Print('>>> [Teleporte] Caso 1: Loading detectado.');
      Teleportou := True;
      while (Engine.Status <> lsOnline) and (GetTickCount - TempoInicial < (TimeoutSegundos * 1000)) do
        Delay(500);
      Break;
    end;

    // Caso 2: Já estou longe da posição memorizada (Mesmo que o teleport tenha sido instantâneo)
    Dist := LUser.DistTo(FMemX, FMemY, FMemZ);
    if (Dist > 4000) then
    begin
      Print('>>> [Teleporte] Caso 2: Posição atual diverge da original. Dist: ' + FormatFloat('0', Dist));
      Teleportou := True;
      Break;
    end;
    
    Delay(200);
  end;

  // Limpa a memória para o próximo uso
  FMemX := 0; FMemY := 0; FMemZ := 0;

  if Teleportou then
  begin
    Print('>>> [Teleporte] Finalizado com sucesso.');
    Delay(2000); 
  end else begin
    Print('>>> [Teleporte] Erro: Timeout.');
  end;
end;

procedure TNPCManager.Interagir(NomeNPC: string; P1: Integer; P2: Integer; P3: Integer);
var
  NPC: TL2Npc;
  LUser: TL2User;
  Tentativas: Integer;
  S: string;
begin
  if not Engine.GetNpcList.ByName(NomeNPC, NPC) then begin
    Print('>>> [Erro] NPC nao encontrado: ' + NomeNPC);
    Exit;
  end;

  LUser := Engine.GetUser;
  if (LUser = nil) then Exit;

  // 1. Memoriza a posição ATUAL antes de começar a clicar nos diálogos
  FMemX := LUser.X; 
  FMemY := LUser.Y; 
  FMemZ := LUser.Z;

  if LUser.DistTo(NPC) > 150 then begin
    Engine.MoveTo(NPC.X, NPC.Y, NPC.Z);
    while (LUser.Moved) and (LUser.DistTo(NPC) > 120) do Delay(100);
  end;

  Engine.SetTarget(NPC);
  Delay(500);

  if not Engine.DlgOpen() then begin
    Engine.UseAction(0); 
    Delay(800);
  end;

  // Processa Bypasses
  if (P1 > 0) and (Engine.DlgText <> '') then begin
    S := Engine.DlgText;
    if ClicarBypass(P1) then begin
      Tentativas := 0;
      repeat Delay(450); Inc(Tentativas);
      until (Engine.DlgText <> S) or (Engine.DlgText = '') or (Tentativas > 10);
    end;
  end;

  if (P2 > 0) and (Engine.DlgText <> '') then begin
    S := Engine.DlgText;
    if ClicarBypass(P2) then begin
      Tentativas := 0;
      repeat Delay(450); Inc(Tentativas);
      until (Engine.DlgText <> S) or (Engine.DlgText = '') or (Tentativas > 10);
    end;
  end;

  if (P3 > 0) and (Engine.DlgText <> '') then begin
    S := Engine.DlgText;
    if ClicarBypass(P3) then begin
      Tentativas := 0;
      repeat Delay(450); Inc(Tentativas);
      until (Engine.DlgText <> S) or (Engine.DlgText = '') or (Tentativas > 10);
    end;
  end;
end;

procedure TNPCManager.QuestRosella;  begin Interagir('Rosella', 1, 1, 0); end;
procedure TNPCManager.QuestGreenis;  begin Interagir('Greenis', 2, 1, 0); end;
procedure TNPCManager.QuestMirabel;  begin Interagir('Teleport Device', 1, 5, 10); end;
procedure TNPCManager.QuestThalia;   begin Interagir('Thalia', 2, 1, 0); end;

initialization
  NPCManager := TNPCManager.Create;

finalization
  if Assigned(NPCManager) then
    FreeAndNil(NPCManager);

end.