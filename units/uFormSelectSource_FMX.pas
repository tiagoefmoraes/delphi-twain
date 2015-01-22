unit uFormSelectSource_FMX;

{$I DelphiTwain.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.Objects
  {$IFDEF DELPHI_XE4_UP}, FMX.StdCtrls{$ENDIF}
  {$IFDEF DELPHI_XE5_UP}, FMX.Graphics{$ENDIF}
  ;

type
  TFormSelectSource = class(TForm)
  protected
    {$IFDEF DELPHI_XE5_UP}
    procedure DoShow; override;
    {$ENDIF}
  public
    {$IFDEF DELPHI_XE5_UP}
    LblCaption: TLabel;
    {$ENDIF}
    LBSources: TListBox;
    BtnOk: TButton;
    BtnCancel: TButton;
    procedure LBSourcesDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent;
      Dummy: {$IFDEF DELPHI_XE5_UP}NativeInt{$ELSE}Integer{$ENDIF} = 0); override;
  end;

implementation

uses DelphiTwainLang;

constructor TFormSelectSource.CreateNew(AOwner: TComponent;
  Dummy: {$IFDEF DELPHI_XE5_UP}NativeInt{$ELSE}Integer{$ENDIF} = 0);
begin
  inherited;

  Caption := DELPHITWAIN_SelectSource;
  Position := TFormPosition.poOwnerFormCenter;
  Width := 300;
  Height := 200;
  BorderIcons := [TBorderIcon.biSystemMenu];

  {$IFDEF DELPHI_XE5_UP}
  LblCaption := TLabel.Create(Self);
  LblCaption.Parent := Self;
  LblCaption.Text := Caption;
  LblCaption.Font.Size := 15;
  LblCaption.StyledSettings := LblCaption.StyledSettings - [TStyledSetting.ssFamily, TStyledSetting.ssSize];
  LblCaption.Position.X := 3;
  LblCaption.Position.Y := 3;
  {$ENDIF}

  LBSources := TListBox.Create(Self);
  LBSources.Parent := Self;
  LBSources.OnDblClick := LBSourcesDblClick;

  BtnOk := TButton.Create(Self);
  BtnOk.Parent := Self;
  BtnOk.ModalResult := mrOk;
  {$IFDEF DELPHI_XE3_UP}
  BtnOk.Text := DELPHITWAIN_OK;
  {$ELSE}
  BtnOk.Text := StringReplace(DELPHITWAIN_OK, '&', '', [rfReplaceAll]);
  {$ENDIF}
  BtnOk.Default := True;

  BtnCancel := TButton.Create(Self);
  BtnCancel.Parent := Self;
  BtnCancel.ModalResult := mrCancel;
  {$IFDEF DELPHI_XE3_UP}
  BtnCancel.Text := DELPHITWAIN_Cancel;
  {$ELSE}
  BtnCancel.Text := StringReplace(DELPHITWAIN_Cancel, '&', '', [rfReplaceAll]);
  {$ENDIF}

  OnResize := FormResize;
  FormResize(nil);
end;

procedure TFormSelectSource.LBSourcesDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

{$IFDEF DELPHI_XE5_UP}
procedure TFormSelectSource.DoShow;
begin
  inherited;

  Width := 300;//BUG WORKAROUND: XE5
  Height := 200;//BUG WORKAROUND: XE5

  //BUG WORKAROUND XE5 -> FORM HEADER IS NOT SHOWN, SET COLOR BACKGROUND AT LEAST
  Fill.Kind := TBrushKind.bkSolid;
  Fill.Color := $FFDDDDFF;
end;
{$ENDIF}

procedure TFormSelectSource.FormResize(Sender: TObject);
begin
  LBSources.Position.X := 3;
  LBSources.Position.Y := 3 {$IFDEF DELPHI_XE5_UP}+ LblCaption.Height + 3{$ENDIF};
  LBSources.Width := Self.ClientWidth - 6;
  LBSources.Height := Self.ClientHeight - (41+LBSources.Position.Y);

  BtnCancel.Position.X := Self.ClientWidth - BtnCancel.Width - 6;
  BtnOK.Position.X := BtnCancel.Position.X - BtnOK.Width - 6;
  BtnCancel.Position.Y := LBSources.ParentedRect.Bottom + (41 - BtnCancel.Height) / 2;
  BtnOK.Position.Y := BtnCancel.Position.Y;
end;

end.
