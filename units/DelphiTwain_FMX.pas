unit DelphiTwain_FMX;

{$I DelphiTwain.inc}

interface

uses
  SysUtils, Classes, DelphiTwain, Twain, Messages, Windows, FMX.Types, UITypes,
  FMX.Forms
  {$IFDEF DELPHI_XE5_UP}, FMX.Graphics{$ENDIF}
  ;

type
  TOnTwainAcquire = procedure(Sender: TObject; const Index: Integer;
    Image: TBitmap; var Cancel: Boolean) of object;
  TOnAcquireProgress = procedure(Sender: TObject; const Index: Integer;
    const Current, Total: Integer) of object;

  TDelphiTwain = class(TCustomDelphiTwain)
  private
    fMessagesTimer: TTimer;

    procedure DoMessagesTimer(Sender: TObject);
    procedure WndProc(var Message: TMessage);
  private
    fOnTwainAcquire: TOnTwainAcquire;
    fOnAcquireProgress: TOnAcquireProgress;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure MessageTimer_Enable; override;
    procedure MessageTimer_Disable; override;
    function CustomSelectSource: Integer; override;
    function CustomGetParentWindow: TW_HANDLE; override;

    procedure DoTwainAcquire(Sender: TObject; const Index: Integer; Image:
      HBitmap; var Cancel: Boolean); override;
    procedure DoAcquireProgress(Sender: TObject; const Index: Integer;
      const Image: HBitmap; const Current, Total: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    {Image acquired}
    property OnTwainAcquire: TOnTwainAcquire read fOnTwainAcquire
      write fOnTwainAcquire;
    {Acquire progress, for memory transfers}
    property OnAcquireProgress: TOnAcquireProgress read fOnAcquireProgress
      write fOnAcquireProgress;
  end;

implementation

{ TDelphiTwain }

uses uFormSelectSource_FMX;

function ColorToAlpha(const aColor: Cardinal): TAlphaColor; inline;
begin
  Result := $FF000000 or (($FF0000 and aColor) shr 16) or (($00FF00 and aColor)) or (($0000FF and aColor) shl 16);
end;

procedure HBitmapToTBitmap(aHBitmap: HBitmap; aFMXBitmap: TBitmap);
var
  xWinBM: Windows.Bitmap;
  xDC: HDC;
  X, Y: Integer;
  {$IFDEF DELPHI_XE3_UP}
  xBMData: TBitmapData;
  {$ENDIF}
begin
  GetObject(aHBitmap, SizeOf(xWinBM), @xWinBM);

  aFMXBitmap.Width := xWinBM.bmWidth;
  aFMXBitmap.Height := xWinBM.bmHeight;

  {$IFDEF DELPHI_XE3_UP}
  aFMXBitmap.Map(TMapAccess.maWrite, xBMData);
  {$ENDIF}
  xDC := CreateCompatibleDC(0);
  try
    SelectObject(xDC, aHBitmap);

    for X := 0 to aFMXBitmap.Width-1 do
    for Y := 0 to aFMXBitmap.Height-1 do begin
      {$IFDEF DELPHI_XE3_UP}
      xBMData.SetPixel(X, Y, ColorToAlpha(GetPixel(xDC, X, Y)));
      {$ELSE}
      aFMXBitmap.StartLine[X + (Y * aFMXBitmap.Width)] := ColorToAlpha(GetPixel(xDC, X, Y));//slow, but sorry...
      {$ENDIF}
    end;
  finally
    ReleaseDC(0, xDC);
    {$IFDEF DELPHI_XE3_UP}
    aFMXBitmap.UnMap(xBMData);
    {$ELSE}
    aFMXBitmap.UpdateHandles;
    aFMXBitmap.BitmapChanged;
    {$ENDIF}
  end;
end;

constructor TDelphiTwain.Create;
begin
  inherited Create;

  fMessagesTimer := TTimer.Create(nil);
  fMessagesTimer.Enabled := False;
  fMessagesTimer.Interval := 100;
  fMessagesTimer.OnTimer := DoMessagesTimer;
end;

function TDelphiTwain.CustomGetParentWindow: TW_HANDLE;
begin
  Result := 0;
end;

function TDelphiTwain.CustomSelectSource: Integer;
var
  xForm: TFormSelectSource;
  I: Integer;
begin
  Result := -1;
  if SourceCount = 0 then begin
    Exit;
  end;

  xForm := TFormSelectSource.CreateNew(nil);
  try
    for I := 0 to SourceCount-1 do
      xForm.LBSources.Items.Add(Source[I].ProductName);

    xForm.LBSources.ItemIndex := 0;
    if (SelectedSourceIndex >= 0) and (SelectedSourceIndex < xForm.LBSources.Items.Count) then
      xForm.LBSources.ItemIndex := SelectedSourceIndex;

    if xForm.ShowModal = mrOK then begin
      Result := xForm.LBSources.ItemIndex;
    end else begin
      Result := -1;
    end;
  finally
    xForm.Free;
  end;
end;

destructor TDelphiTwain.Destroy;
begin
  FreeAndNil(fMessagesTimer);

  inherited;
end;

procedure TDelphiTwain.DoAcquireProgress(Sender: TObject; const Index: Integer;
  const Image: HBitmap; const Current, Total: Integer);
begin
  if Assigned(fOnAcquireProgress) then
    fOnAcquireProgress(Self, Index, Current, Total);
end;

procedure TDelphiTwain.DoMessagesTimer(Sender: TObject);
begin
  //MUST BE HERE SO THAT TWAIN RECEIVES MESSAGES
  if VirtualWindow > 0 then begin
    SendMessage(VirtualWindow, WM_USER, 0, 0);
  end;
end;

procedure TDelphiTwain.DoTwainAcquire(Sender: TObject; const Index: Integer;
  Image: HBitmap; var Cancel: Boolean);
var
  xBmp: TBitmap;
begin
  if Assigned(OnTwainAcquire) then
  begin
    xBmp := TBitmap.Create(0, 0);
    try
      HBitmapToTBitmap(Image, xBmp);
      fOnTwainAcquire(Sender, Index, xBmp, Cancel);
    finally
      xBmp.Free;
    end;
  end;
end;

procedure TDelphiTwain.MessageTimer_Disable;
begin
  if Assigned(fMessagesTimer) then
    fMessagesTimer.Enabled := False;
end;

procedure TDelphiTwain.MessageTimer_Enable;
begin
  if Assigned(fMessagesTimer) then
    fMessagesTimer.Enabled := True;
end;

procedure TDelphiTwain.DoCreate;
begin
  inherited;

  fVirtualWindow := Classes.AllocateHWnd(WndProc);
end;

procedure TDelphiTwain.DoDestroy;
begin
  DestroyWindow(VirtualWindow);

  inherited;
end;

procedure TDelphiTwain.WndProc(var Message: TMessage);
var
  i    : Integer;
  xMsg  : TMsg;
begin
  //WndProc := False;
  with Message do begin
  {Tests for the message}
    {Try to obtain the current object pointer}
    if Assigned(Self) then
      {If there are sources loaded, we need to verify}
      {this message}
      if (Self.SourcesLoaded > 0) then
      begin
        {Convert parameters to a TMsg}
        xMsg := MakeMsg(Handle, Msg, wParam, lParam);//MakeMsg(Handle, Msg, wParam, lParam);
        {Tell about this message}
        FOR i := 0 TO Self.SourceCount - 1 DO
          if ((Self.Source[i].Loaded) and (Self.Source[i].Enabled)) then
            if Self.Source[i].ProcessMessage(xMsg) then
            begin
              {Case this was a message from the source, there is}
              {no need for the default procedure to process}
              //Result := 0;
              //WndProc := True;
              Exit;
            end;

      end; {if (Twain.SourcesLoaded > 0)}
  end;
end;

end.
