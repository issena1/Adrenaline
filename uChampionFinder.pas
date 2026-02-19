unit uChampionFinder;

interface

uses SysUtils, Classes;

type
  TChampionFinder = class
  private
    FChampOID       : Cardinal;
    FFaceControlPaused: Boolean; 
    
    function FindChampion(Raio: Integer): TL2Npc; 
  public
    // Configurações
    FSkillID        : Integer; 
    FDistanciaSkill : Integer;
    
    constructor Create; 
    
    // Métodos de Controle
    procedure SetSkillID(SkillID: Integer);
    function SetTargetNPC(Raio: Integer): TL2Npc; 
    procedure RunChampionPriority(RaioBusca: Integer = 1500; IntervaloMs: Integer = 500);
    procedure Iniciar; 
  end;

var
  GChampion: TChampionFinder;

procedure GChampionThreadBridge; 

implementation

procedure GChampionThreadBridge;
begin
  if GChampion <> nil then
    GChampion.RunChampionPriority(1500, 500); 
end;

constructor TChampionFinder.Create;
begin
  inherited;
  FSkillID           := 1175; 
  FDistanciaSkill    := 600;  
  FFaceControlPaused := False; 
end;

procedure TChampionFinder.SetSkillID(SkillID: Integer);
begin
  FSkillID := SkillID;
end;

procedure TChampionFinder.Iniciar;
begin
  Script.NewThread(@GChampionThreadBridge);
  Print('>>> [ChampionFinder] Thread de prioridade iniciada!');
end;

function TChampionFinder.FindChampion(Raio: Integer): TL2Npc;
var
  i: Integer;
  NPC: TL2Npc;
  Dist, MenorDist: Double;
begin
  Result := nil;
  MenorDist := Raio + 1; 

  if (User = nil) or (not User.valid) then Exit;
  if NpcList.Count = 0 then Exit;

  for i := 0 to NpcList.Count - 1 do
  begin
    NPC := NpcList.Items(i);
    if (NPC = nil) or (not NPC.valid) or NPC.Dead or (NPC.HP <= 0) then Continue;
    if not NPC.Attackable then Continue;
    if (NPC.Team = 0) then Continue; 

    Dist := User.DistTo(NPC);
    if Dist < MenorDist then
    begin
      MenorDist := Dist;
      Result := NPC; 
    end;
  end;
end;

function TChampionFinder.SetTargetNpc(Raio: Integer): TL2Npc;
var
  Target: TL2Npc;
begin
  Result := nil;
  Target := FindChampion(Raio);

  if (Target <> nil) and Target.valid then
  begin
    if (User.Target = nil) or (User.Target.OID <> Target.OID) then
    begin
      Engine.SetTarget(Target);
      Print('>>> Champion Focado: ' + Target.Name);
    end;
    Result := Target; 
  end;
end;

procedure TChampionFinder.RunChampionPriority(RaioBusca: Integer; IntervaloMs: Integer);
var
  Champion: TL2Npc;
  Dist: Double;
  TargetOID: Cardinal;
begin
  while True do
  begin
    Delay(IntervaloMs);

    if (User = nil) or (not User.valid) or (User.Dead) then Continue;

    // 1. Busca Champion
    Champion := SetTargetNPC(RaioBusca);

    if (Champion <> nil) and Champion.valid then
    begin
      // --- FASE 1: VERIFICAÇÃO DE ALVO ---
      // Obtém o OID do alvo atual do jogador (se houver)
      TargetOID := 0;
      if (User.Target <> nil) and User.Target.valid then
        TargetOID := User.Target.OID;

      // Se NÃO estamos focados no Champion OU estamos parados (sem atacar/castar)
      if (TargetOID <> Champion.OID) or ((User.Cast.EndTime = 0)) then
      begin
         // --- AÇÃO: FORÇAR INÍCIO DO COMBATE ---
         
         // Se o alvo está errado, corrige e pausa o bot momentaneamente
         if (TargetOID <> Champion.OID) then
         begin
           // Pausa o FaceControl de Ataque (ID=1) para não trocar de alvo
           Engine.FaceControl(1, False); 
           Engine.SetTarget(Champion);
           Print('>>> [Champion] Focando Alvo: ' + Champion.Name);
           Delay(200); // Delay para o servidor registrar a troca de target
         end;

         Dist := User.DistTo(Champion);

         // --- FASE 2: APROXIMAÇÃO (Se necessário) ---
         if Dist > FDistanciaSkill then
         begin
           // Move até entrar no range (com margem de 50 units)
           // Usamos valor NEGATIVO para indicar movimento na direção do alvo
           Engine.MoveToTarget(-(FDistanciaSkill - 50)); 
           Delay(300); // Espera o movimento iniciar
         end
         
         // --- FASE 3: PRIMEIRO ATAQUE (Start) ---
         else
         begin
           // Estamos no range. Se não estivermos castando nada, dá o primeiro hit.
           if (User.Cast.EndTime = 0) then
           begin
             // Tenta usar a skill configurada (Force=True)
             if Engine.UseSkill(FSkillID, True) then 
             begin
               Print('>>> [Champion] Combate Iniciado! Entregando para FaceControl...');
               // RELIGA O BOT IMEDIATAMENTE APÓS O COMANDO DE SKILL SAIR
               // O delay de 500ms garante que o cast comece antes do bot tentar outra coisa
               Delay(500); 
               Engine.FaceControl(1, True); 
             end;
           end;
         end;
      end
      else
      begin
        // --- FASE 4: DURANTE O COMBATE ---
        // Se já estamos focados no Champion E já estamos lutando (castando/atacando),
        // garantimos que o FaceControl esteja LIGADO para continuar a rotação de skills.
        Engine.FaceControl(1, True); 
      end;
    end
    else
    begin
      // --- SEM CHAMPION ---
      // Se não achou Champion, garante que o bot esteja rodando normal para farmar outros mobs.
      Engine.FaceControl(1, True);
    end;
  end;
end;



initialization
  GChampion := TChampionFinder.Create;

finalization
  if GChampion <> nil then GChampion.Free;

end.
