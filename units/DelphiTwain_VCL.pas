unit DelphiTwain_VCL;

{$I DelphiTwain.inc}

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, Forms, ExtCtrls, Messages, Graphics,
  {$IFDEF FPC}interfacebase,{$ENDIF}
  DelphiTwain, Twain;

type
  TOnTwainAcquire = procedure(Sender: TObject; const Index: Integer; Image:
    TBitmap; var Cancel: Boolean) of object;
  TOnAcquireProgress = procedure(Sender: TObject; const Index: Integer;
    const Image: HBitmap; const Current, Total: Integer) of object;

  TDelphiTwain = class(TCustomDelphiTwain)
  private
    fMessagesTimer: TTimer;

    procedure DoMessagesTimer(Sender: TObject);
    procedure WndProc(var Message: TMessage);
    function WndFunc(var Message: TMessage): Boolean;
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

uses uFormSelectSource_VCL, Controls;

{$IFDEF FPC}
var
  xTwainList: TList = nil;
  xAppWndCallback: WNDPROC = nil;//WNDPROC = nil;

function AppWndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
var
  I: Integer;
  xMess: TMessage;
begin
  if Assigned(xTwainList) and (xTwainList.Count > 0) then begin
    xMess.msg := uMsg;
    xMess.lParam := lParam;
    xMess.wParam := wParam;
    xMess.Result := 0;

    for I := 0 to xTwainList.Count-1 do
      TDelphiTwain(xTwainList[I]).WndProc(xMess);
  end;
  Result := CallWindowProc(xAppWndCallback,Ahwnd, uMsg, WParam, LParam);
end;
{$ENDIF}

{ TDelphiTwain }

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
  if IsConsoleApplication then
    Result := 0
  else if not IsLibrary then
    Result := {$IF DEFINED(FPC) OR DEFINED(DELPHI_7_DOWN)}GetActiveWindow{$ELSE}Application.ActiveFormHandle{$IFEND}
  else
    Result := GetActiveWindow;//GetForegroundWindow;
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

    {$IFDEF DELPHI_2006_UP}
    xForm.PopupMode := pmAuto;
    {$ENDIF}
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
    fOnAcquireProgress(Self, Index, Image, Current, Total);
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
var BitmapObj: TBitmap;
begin
  if Assigned(OnTwainAcquire) then
  begin
    BitmapObj := TBitmap.Create;
    try
      BitmapObj.Handle := Image;
      OnTwainAcquire(Sender, Index, BitmapObj, Cancel);
    finally
      BitmapObj.Free;
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

  if IsLibrary then begin
    fVirtualWindow := Classes.AllocateHWnd(WndProc);
  end else begin
    {$IFDEF FPC}
    if Assigned(Application.MainForm) and (Application.MainForm.Visible) then
      fVirtualWindow := Application.MainFormHandle
    else
      fVirtualWindow := WidgetSet.AppHandle;

    if not Assigned(xTwainList) then
      xTwainList := TList.Create;
    xTwainList.Add(Self);
    if not Assigned(xAppWndCallback) then begin
      xAppWndCallback := {%H-}{Windows.WNDPROC}Pointer(SetWindowLongPtr(fVirtualWindow,GWL_WNDPROC,{%H-}NativeInt(@AppWndCallback)));
    end;

    {$ELSE}
    fVirtualWindow := Application.Handle;//Application.Handle;
    Application.HookMainWindow(WndFunc);
    {$ENDIF}
  end;
end;

procedure TDelphiTwain.DoDestroy;
begin
  if IsLibrary then begin
    DestroyWindow(VirtualWindow);
  end else begin
    {$IFDEF FPC}
    xTwainList.Remove(Self);
    if xTwainList.Count = 0 then begin
      FreeAndNil(xTwainList);
    end;
    {$ELSE}
    Application.UnhookMainWindow(WndFunc);
    {$ENDIF}
  end;

  inherited;
end;

function TDelphiTwain.WndFunc(var Message: TMessage): Boolean;
var
  i    : Integer;
  xMsg  : TMsg;
begin
  Result := False;
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
                Result := 0;
                WndFunc := True;
                Exit;
              end;

        end; {if (Twain.SourcesLoaded > 0)}
  end;
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
