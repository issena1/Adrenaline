unit GPS;

interface

uses SysUtils, Classes;

type
  TGPSRecorder = class
  private
    FArquivoDestino: string;
    FLastX, FLastY, FLastZ: Integer;
    FCapturando: Boolean;
    FSalvando: Boolean;
    FBufferPontos: TStringList;
    FLastHeartbeat: Cardinal;
    FEstavaMovendo: Boolean;
    FSalvarPendente: Boolean; // sinaliza para a thread salvar no próximo ciclo
    function FormatarPonto(x, y, z: Integer; Motivo: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Iniciar(NomeArquivo: string = '');
    procedure Parar;
    procedure SalvarNoDisco;
    procedure ExecutarLoop;
    property Capturando: Boolean read FCapturando;
  end;

var
  GRecorder: TGPSRecorder;

procedure GpsThreadBridge;

implementation

procedure GpsThreadBridge;
begin
  if GRecorder <> nil then
    GRecorder.ExecutarLoop;
end;

constructor TGPSRecorder.Create;
begin
  inherited Create;
  FBufferPontos  := TStringList.Create;
  FCapturando    := False;
  FSalvando      := False;
  FSalvarPendente := False;
  FLastHeartbeat := 0;
  FEstavaMovendo := False;
end;

destructor TGPSRecorder.Destroy;
begin
  if FCapturando then Parar;
  if Assigned(FBufferPontos) then begin
    if FBufferPontos.Count > 0 then SalvarNoDisco;
    FBufferPontos.Free;
    FBufferPontos := nil;
  end;
  inherited Destroy;
end;

function TGPSRecorder.FormatarPonto(x, y, z: Integer; Motivo: string): string;
begin
  Result := Format('X:%d Y:%d Z:%d | %-9s | %s',
    [x, y, z, Motivo, FormatDateTime('dd/mm/yyyy hh:nn:ss', Now)]);
end;

procedure TGPSRecorder.Iniciar(NomeArquivo: string);
var
  Base    : string;
  Tent    : Integer;
  ux, uy, uz: Integer;
  uMov    : Boolean;
begin
  if FCapturando then begin
    Print('>>> [GPS] Ja esta gravando. Chame Parar primeiro.');
    Exit;
  end;

  // Aguarda save anterior terminar
  Tent := 0;
  while FSalvando and (Tent < 20) do begin
    Delay(100);
    Inc(Tent);
  end;

  // Monta caminho
  Base := ExePath;
  if (Base <> '') and (Base[Length(Base)] <> '\') then Base := Base + '\';
  if NomeArquivo = '' then
    NomeArquivo := Format('rota_%s.txt', [FormatDateTime('yyyymmdd_hhnnss', Now)]);
  FArquivoDestino := Base + 'caminhos\' + NomeArquivo;

  // Lê posição inicial com proteção
  if User = nil then begin
    Print('>>> [GPS] Erro: Personagem nao encontrado.');
    Exit;
  end;
  try
    ux   := User.X;
    uy   := User.Y;
    uz   := User.Z;
    uMov := User.Moved;
  except
    Print('>>> [GPS] Erro: Falha ao ler posicao inicial.');
    Exit;
  end;

  FLastX         := ux;
  FLastY         := uy;
  FLastZ         := uz;
  FEstavaMovendo := uMov;

  FBufferPontos.Clear;
  FBufferPontos.Add(FormatarPonto(ux, uy, uz, 'INICIO'));

  FCapturando     := True;
  FSalvarPendente := True; // thread vai salvar no primeiro ciclo
  FLastHeartbeat  := GetTickCount;

  // NÃO chama SalvarNoDisco aqui — deixa para a thread
  Script.NewThread(@GpsThreadBridge);
  Print('>>> [GPS] Gravacao iniciada: ' + NomeArquivo);
end;

procedure TGPSRecorder.ExecutarLoop;
var
  Dist        : Double;
  MovendoAgora: Boolean;
  cx, cy, cz  : Integer;
begin
  while FCapturando do begin
    Delay(200);

    // Salva pendente do Iniciar (primeiro ciclo da thread)
    if FSalvarPendente then begin
      FSalvarPendente := False;
      SalvarNoDisco;
    end;

    if Engine.Status <> lsOnline then Continue;
    if User = nil then Continue;

    try
      cx           := User.X;
      cy           := User.Y;
      cz           := User.Z;
      MovendoAgora := User.Moved;
    except
      Continue;
    end;

    Dist := Sqrt(
      (Int64(cx - FLastX) * Int64(cx - FLastX)) +
      (Int64(cy - FLastY) * Int64(cy - FLastY)) +
      (Int64(cz - FLastZ) * Int64(cz - FLastZ))
    );

    // ── 1. PARADA ──
    if FEstavaMovendo and (not MovendoAgora) then begin
      if (cx <> FLastX) or (cy <> FLastY) then begin
        FBufferPontos.Add(FormatarPonto(cx, cy, cz, 'PARADA'));
        FLastX := cx;
        FLastY := cy;
        FLastZ := cz;
      end else begin
        if FBufferPontos.Count > 0 then begin
          if Pos('CAMINHO', FBufferPontos[FBufferPontos.Count - 1]) > 0 then
            FBufferPontos[FBufferPontos.Count - 1] :=
              FormatarPonto(cx, cy, cz, 'PARADA')
          else
            FBufferPontos.Add(FormatarPonto(cx, cy, cz, 'PARADA'));
        end else
          FBufferPontos.Add(FormatarPonto(cx, cy, cz, 'PARADA'));
      end;
      FEstavaMovendo := False;
      SalvarNoDisco;
      Print('>>> [GPS] Parada detectada. Ponto salvo.');
      Continue;
    end;

    // ── 2. CAMINHO ──
    if MovendoAgora and (Dist > 150) then begin
      FBufferPontos.Add(FormatarPonto(cx, cy, cz, 'CAMINHO'));
      FLastX := cx;
      FLastY := cy;
      FLastZ := cz;
    end;

    FEstavaMovendo := MovendoAgora;

    // ── 3. Heartbeat ──
    if (GetTickCount - FLastHeartbeat > 30000) then
      FLastHeartbeat := GetTickCount;
  end;
end;

procedure TGPSRecorder.SalvarNoDisco;
var
  ListaTemp, Copia: TStringList;
begin
  if FSalvando then Exit;
  if (FBufferPontos = nil) or (FBufferPontos.Count = 0) then Exit;
  if FArquivoDestino = '' then Exit;

  FSalvando := True;
  Copia := TStringList.Create;
  try
    Copia.AddStrings(FBufferPontos);
    FBufferPontos.Clear;

    ListaTemp := TStringList.Create;
    try
      try
        if FileExists(FArquivoDestino) then
          ListaTemp.LoadFromFile(FArquivoDestino);
        ListaTemp.AddStrings(Copia);
        ListaTemp.SaveToFile(FArquivoDestino);
      except
        FBufferPontos.AddStrings(Copia); // restaura se falhou
      end;
    finally
      ListaTemp.Free;
    end;
  finally
    Copia.Free;
    FSalvando := False;
  end;
end;

procedure TGPSRecorder.Parar;
var
  Tent: Integer;
  px, py, pz: Integer;
begin
  if not FCapturando then Exit;

  FCapturando := False;

  Tent := 0;
  while FSalvando and (Tent < 25) do begin
    Delay(100);
    Inc(Tent);
  end;
  Delay(250);

  // Grava posição final como PARADA com proteção
  if User <> nil then begin
    try
      px := User.X;
      py := User.Y;
      pz := User.Z;
      FBufferPontos.Add(FormatarPonto(px, py, pz, 'PARADA'));
    except
      // ignora se User foi invalidado
    end;
  end;

  SalvarNoDisco;
  Print('>>> [GPS] Gravacao finalizada e salva: ' + FArquivoDestino);
end;

initialization
  GRecorder := TGPSRecorder.Create;

finalization
  if Assigned(GRecorder) then
    FreeAndNil(GRecorder);

end.
