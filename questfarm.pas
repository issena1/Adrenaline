unit questFarm;

interface

uses
  SysUtils, Classes;

procedure FarmarComFaceControl(ItemNome: string; QtdNecessaria: Integer);
function ObterQtdQuestItem(Nome: string): Int64;
procedure unstuck();

implementation

function ObterQtdQuestItem(Nome: string): Int64;
var
  Item: TL2Item;
begin
  // Sempre use GetInventory para garantir dados atualizados
  if Engine.GetInventory.Quest.ByName(Nome, Item) then
    Result := Item.Count
  else
    Result := 0;
end;

procedure FarmarComFaceControl(ItemNome: string; QtdNecessaria: Integer);
var
  QtdAtual: Int64;
  LUser: TL2User;
begin
  QtdAtual := ObterQtdQuestItem(ItemNome);
  
  if QtdAtual >= QtdNecessaria then
  begin
    Print('>>> Voce ja possui os itens necessarios.');
    Engine.FaceControl(0, False);
    if ItemNome = 'Resonance Amulet: 2' then Exit; //Amuleto de ressonancia é da 3job
    unstuck();
    Exit;
  end;

  Print('>>> Ativando FaceControl para Farm de Quest...');
  Engine.FaceControl(0, True); 

  while (QtdAtual < QtdNecessaria) do
  begin
    LUser := Engine.GetUser;
    if (LUser = nil) or (LUser.Dead) then Break;
    
    Delay(2000);
    QtdAtual := ObterQtdQuestItem(ItemNome);
    Print('>>> Itens coletados: ' + IntToStr(QtdAtual) + ' / ' + IntToStr(QtdNecessaria));
  end;

  Engine.FaceControl(0, False);
  
  LUser := Engine.GetUser;
  if (LUser <> nil) and (not LUser.Dead) then
  begin
    if ItemNome = 'Resonance Amulet: 2' then begin
      Print('>>> Amuleto de Ressonancia coletado! Parando o bot e aguardando proximo passo da quest...');
      exit;
    end
    else
      Print('>>> Farm concluido! Aguardando proximo passo da quest...');
    unstuck();
  end
  else
    Print('>>> Farm interrompido (Morte ou Desconexao).');
end;

procedure unstuck();
var
  MemX, MemY, MemZ: Integer;
  LUser: TL2User;
  TempoSeguranca: Cardinal;
  Saltou: Boolean;
begin
  LUser := Engine.GetUser;
  if (LUser = nil) then Exit;

  // 1. MEMORIZA A POSIÇÃO INICIAL (Onde o boneco está preso ou no farm)
  MemX := LUser.X;
  MemY := LUser.Y;
  MemZ := LUser.Z;
  Saltou := False;

  Print('>>> [Unstuck] Iniciando comando...');
  Engine.EnterText('/unstuck');
  
  // Aguarda o cast iniciar (previne ler EndTime antes do comando subir)
  Delay(1500); 

  // 2. MONITORA O CASTING (30 segundos padrão)
  Print('>>> [Unstuck] Aguardando barra de cast terminar...');
  while (True) do begin
    LUser := Engine.GetUser;
    // Se o cast acabou (EndTime = 0) ou se entramos em Loading (LUser = nil)
    if (LUser = nil) or (LUser.Cast.EndTime = 0) then Break;
    Delay(500);
  end;

  // 3. DETECÇÃO ROBUSTA DE TELEPORTE (Pós-Cast)
  Print('>>> [Unstuck] Cast finalizado. Verificando deslocamento...');
  TempoSeguranca := GetTickCount;
  
  // Verificamos por até 10 segundos se houve mudança de mapa ou posição
  while (GetTickCount - TempoSeguranca < 10000) do
  begin
    LUser := Engine.GetUser;

    // Caso A: Entrou em Loading (Teleporte de longa distância)
    if (Engine.Status <> lsOnline) or (LUser = nil) then
    begin
      Print('>>> [Unstuck] Caso A: Loading detectado.');
      Saltou := True;
      while (Engine.Status <> lsOnline) do Delay(1000);
      Break;
    end;

    // Caso B: Teleporte instantâneo (Detecta se saiu de perto da posição inicial)
    if (LUser.DistTo(MemX, MemY, MemZ) > 3000) then
    begin
      Print('>>> [Unstuck] Caso B: Salto detectado por distancia.');
      Saltou := True;
      Break;
    end;

    Delay(300);
  end;

  // 4. FINALIZAÇÃO E ESTABILIZAÇÃO
  if Saltou then
  begin
    Print('>>> [Unstuck] Sucesso! Aguardando o mapa carregar...');
    
    // Espera os NPCs "nascerem" para confirmar que o boneco já pode agir
    TempoSeguranca := GetTickCount;
    while (Engine.GetNpcList.Count = 0) and (GetTickCount - TempoSeguranca < 8000) do
      Delay(500);
      
    Delay(2000); // Estabilidade final
    Print('>>> [Unstuck] Personagem reposicionado e pronto.');
  end else begin
    Print('>>> [Unstuck] Erro: O tempo acabou e o personagem nao se moveu.');
  end;
end;

end.
