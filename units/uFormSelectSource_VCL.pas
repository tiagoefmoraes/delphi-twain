unit uFormSelectSource_VCL;

{$I DelphiTwain.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Types, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls;

type
  TFormSelectSource = class(TForm)
  protected
    procedure DoCreate; override;
  public
    LBSources: TListBox;
    PnlBottom: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure FormResize(Sender: TObject);
    procedure LBSourcesDblClick(Sender: TObject);
  end;

implementation

uses DelphiTwainLang;

procedure TFormSelectSource.DoCreate;
var
  xM: TMonitor;
  xR: TRect;
begin
  inherited;

  Caption := DELPHITWAIN_SelectSource;
  Position := poDesigned;
  Width := 300;
  Height := 200;
  xM := Screen.MonitorFromPoint(Mouse.CursorPos);
  xR := xM.WorkareaRect;
  Left := (xR.Left+xR.Right-Width) div 2;
  Top := (xR.Top+xR.Bottom-Height) div 2;
  BorderIcons := [biSystemMenu];

  LBSources := TListBox.Create(Self);
  LBSources.Parent := Self;
  LBSources.Align := alClient;
  {$IFDEF DELPHI_2009_UP}
  LBSources.AlignWithMargins := True;
  {$ENDIF}
  LBSources.OnDblClick := LBSourcesDblClick;

  PnlBottom := TPanel.Create(Self);
  PnlBottom.Parent := Self;
  PnlBottom.Height := 41;
  PnlBottom.Top := Self.Height;
  PnlBottom.Align := alBottom;
  PnlBottom.BevelOuter := bvNone;

  BtnOk := TButton.Create(Self);
  BtnOk.Parent := PnlBottom;
  BtnOk.ModalResult := mrOk;
  BtnOk.Caption := DELPHITWAIN_OK;
  BtnOk.Default := True;

  BtnCancel := TButton.Create(Self);
  BtnCancel.Parent := PnlBottom;
  BtnCancel.ModalResult := mrCancel;
  BtnCancel.Caption := DELPHITWAIN_Cancel;

  OnResize := FormResize;
  FormResize(nil);
end;

procedure TFormSelectSource.LBSourcesDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormSelectSource.FormResize(Sender: TObject);
begin
  BtnCancel.Left := PnlBottom.ClientWidth - BtnCancel.Width - 6;
  BtnOK.Left := BtnCancel.Left - BtnOK.Width - 6;
  BtnCancel.Top := (PnlBottom.ClientHeight - BtnCancel.Height) div 2;
  BtnOK.Top := BtnCancel.Top;
end;

end.
