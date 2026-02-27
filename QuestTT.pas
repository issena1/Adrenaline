uses lerdados, bypass, questfarm, SysUtils;

var EstagioQuestManual, Ciclo: Integer;
    local: string;

const caminho1 = 'Goddard.txt';
      caminho2 = 'Jeremy.txt';
      caminho3 = 'Beolin.txt';
      caminho4 = 'Kuber.txt';
      caminho5 = 'Crocus.txt';
      caminho6 = 'Naff.txt';
      caminho7 = 'Pulin.txt';
      caminho8 = 'Lietta.txt';

begin

    EstagioQuestManual := 0;
    local := '';
    Ciclo := 0;
    SKILL_ID := 1235; //Aguinha

    if SeguirCaminhoGravado(caminho1) then
    begin
        Print('Rebuff e Seguindo');
        if NPCManager.Interagir('Agatha Christie', [12]) then
        begin
            Print('Rebuff e Interagiu com Agatha Christie, aguardando teleporte...');
        end
        else
            Print('Nao conseguiu interagir com Agatha Christie');
    end
    else
        Print('Nao esta em Goddard');
    // Parte 1 - Tatiana Gk
    if User.InRange(147911, -55266, -2728, 200) then
    begin
        Print('>>> [Parte 1] Indo para Hotspring...');
        NPCManager.Interagir('Tatiana', [1, 11]);
        NPCManager.AguardarTeleporte(20);
        Print('Chegou em Hotspring!');
    end;
    if EstagioQuestManual = 0 then
    begin
        // Parte 2 - Jeremy
        if SeguirCaminhoGravado(caminho2) then
            Print('>>> [Parte 2] Indo para Jeremy...')
        else
            Print('Nao esta em Hotspring Respown');
    
        if (NPCManager.InteragirPorNome('Jeremy', ['quest', 'Liquor Delivery', 'deliver'])) then
        begin
            Print('Interagiu com Jeremy, aguardando teleporte...');
            Inc(EstagioQuestManual);
        end
        else
            Print('Nao conseguiu interagir com Jeremy ou quest ja iniciada');
    end
    else
        Print('>>> Nao esta no estagio correto para essa parte.');

    // Parte 3 - Beolin
    if EstagioQuestManual = 1 then
    begin
        if SeguirCaminhoGravado(caminho3) then
            Print('>>> [Parte 3] Indo para Beolin...')
        else
            Print('Nao esta em Jeremy');

        if NPCManager.Interagir('Beolin', [2, 1]) then
        begin
            Print('Interagiu com Beolin, aguardando teleporte...');
            Inc(EstagioQuestManual);
        end
        else
            Print('Nao conseguiu interagir com Beolin ou quest ja iniciada');

    end  
    else
        Print('>>> Nao esta no estagio correto para essa parte.');


    // Parte 4 - Kuber
    if EstagioQuestManual = 2 then
    begin
        Print('Beolin interagido, indo para o proximo passo da quest...');

        if SeguirCaminhoGravado(caminho4) then
            Print('>>> [Parte 4] Indo para Kuber...')
        else
            Print('Nao esta em Beolin');

        if NPCManager.Interagir('Kuber', [2, 1]) then
        begin
            Print('Interagiu com Kuber, Indo para o proximo passo da quest...');
            Inc(EstagioQuestManual);
        end
        else
            Print('Nao conseguiu interagir com Kuber ou quest ja iniciada');

    end  
    else
        Print('>>> Nao esta no estagio correto para essa parte.');


    // Parte 5 - Crocus
    if EstagioQuestManual = 3 then
    begin
        Print('>>> Indo para o proximo passo da quest...');
        if SeguirCaminhoGravado(caminho5) then
        begin
            Print('>>> [Parte 5] Indo para Hotspring...');
        end
        else
            Print('Nao esta em Kuber');

        if (NPCManager.Interagir('Crocus', [2, 1])) then
        begin
            Print('Interagiu com Crocus, Indo para o proximo passo da quest...');
            Inc(EstagioQuestManual);
        end
        else
        begin
            Print('Nao conseguiu interagir com Crocus ou quest ja iniciada');
        end;
    end
    else
    begin
        Print('>>> Não pode pegar a quest com Kuber. Ou quest ja iniciada.');
    end;

    // Parte 6 - Naff
    if EstagioQuestManual = 4 then
    begin
        Print('>>> Indo para o proximo passo da quest, falar com Naff...');

        if SeguirCaminhoGravado(caminho6) then
            Print('>>> [Parte 6] Indo para Naff...')
        else
            Print('Nao esta em Crocus');

        if (NPCManager.Interagir('Naff', [2, 1])) then
        begin
            Print('Interagiu com Naff, Concluindo quest...');
            Inc(EstagioQuestManual);
        end
        else
            Print('Nao conseguiu interagir com Naff ou quest ja iniciada');
    end
    else
    begin
        Print('>>> Não pode pegar a quest com Crocus. Ou quest ja iniciada.');
    end;
    // Parte 7 - Pulin
    if EstagioQuestManual = 5 then
    begin
        Print('>>> Indo para o proximo passo da quest...');

        if SeguirCaminhoGravado(caminho7) then
            Print('>>> [Parte 7] Indo para Pulin...')
        else
            Print('Nao esta em Naff');

        if (NPCManager.Interagir('Pulin', [2, 1])) then
        begin
            Print('Interagiu com Pulin, Proseguidno a quest...');
            Inc(EstagioQuestManual);
        end
        else
            Print('Nao conseguiu interagir com Pulin ou quest ja iniciada');
    end
    else    begin
        Print('>>> Não pode pegar a quest com Naff. Ou quest ja iniciada.');
    end;

    //Voltando para Jeremy
    if EstagioQuestManual = 6 then
    begin
        Print('>>> Indo para o proximo passo da quest...');

        if SeguirCaminhoGravado(caminho2) then
            Print('>>> [Parte 8] Indo para Jeremy...')
        else
            Print('Nao esta em Pulin');

        if (NPCManager.Interagir('Jeremy', [1, 1, 1])) then
        begin
            Print('Interagiu com Jeremy, Concluindo quest...');
            Inc(EstagioQuestManual);
        end
        else
            Print('Nao conseguiu interagir com Jeremy ou quest ja iniciada');
    end
    else    begin
        Print('>>> Não pode pegar a quest com Pulin. Ou quest ja iniciada.');
    end;

   // Parte Final - Voltando para goddard
    if EstagioQuestManual = 7 then
    begin
        Print('>>> Indo para o proximo passo da quest...');

        if User.InRange(149559, -112664, -2064, 200) then
        begin
            Print('Proximo a Jeremy, na ultima parte de goddard, voltando para goddard...');
            unstuck;
        end;

        if SeguirCaminhoGravado(caminho8) then
            Print('>>> [Parte 9] Indo para Lietta...')
        else
            Print('Nao esta em Goddard');

        if NPCManager.Interagir('Lietta', [4, 1]) then
        begin
            Print('Interagiu com Lietta, Quest concluida!');
            Print('Iniciando um novo ciclo de quest...');
            EstagioQuestManual := 0;
            Inc(Ciclo);
            Print('>>> Ciclo atual: ' + IntToStr(Ciclo));
        end
        else
            Print('Nao conseguiu interagir com Lietta');

    end  
    else
        Print('>>> Nao esta no estagio correto para essa parte.');

    
end.
