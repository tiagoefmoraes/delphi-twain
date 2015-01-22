unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DelphiTwain, DelphiTwain_Vcl, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    PnlTop: TPanel;
    LBSources: TListBox;
    ImgHolder: TImage;
    BtnScanWithDialog: TButton;
    BtnScanWithoutDialog: TButton;
    BtnReloadSources: TButton;
    procedure BtnReloadSourcesClick(Sender: TObject);
    procedure BtnScanWithDialogClick(Sender: TObject);
    procedure BtnScanWithoutDialogClick(Sender: TObject);
  private
    Twain: TDelphiTwain;

    procedure ReloadSources;
    procedure TwainTwainAcquire(Sender: TObject; const Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.BtnReloadSourcesClick(Sender: TObject);
begin
  if Twain.SourceManagerLoaded then
    ReloadSources;
end;

procedure TForm1.BtnScanWithDialogClick(Sender: TObject);
begin
  Twain.SelectedSourceIndex := LBSources.ItemIndex;

  if Assigned(Twain.SelectedSource) then begin
    //Load source, select transference method and enable (display interface)}
    Twain.SelectedSource.Loaded := True;
    Twain.SelectedSource.ShowUI := True;//display interface
    Twain.SelectedSource.Enabled := True;
  end;
end;

procedure TForm1.BtnScanWithoutDialogClick(Sender: TObject);
begin
  Twain.SelectedSourceIndex := LBSources.ItemIndex;

  if Assigned(Twain.SelectedSource) then begin
    //Load source, select transference method and enable (display interface)}
    Twain.SelectedSource.Loaded := True;
    Twain.SelectedSource.ShowUI := False;
    Twain.SelectedSource.Enabled := True;
  end;
end;

procedure TForm1.DoCreate;
begin
  inherited;

  Twain := TDelphiTwain.Create;
  Twain.OnTwainAcquire := TwainTwainAcquire;

  if Twain.LoadLibrary then
  begin
    //Load source manager
    Twain.SourceManagerLoaded := TRUE;

    ReloadSources;
  end else begin
    ShowMessage('Twain is not installed.');
  end;
end;

procedure TForm1.DoDestroy;
begin
  Twain.Free;//Don't forget to free Twain!

  inherited;
end;

procedure TForm1.ReloadSources;
var
  I: Integer;
begin
  LBSources.Items.Clear;
  for I := 0 to Twain.SourceCount-1 do
    LBSources.Items.Add(Twain.Source[I].ProductName);

  if LBSources.Items.Count > 0 then
    LBSources.ItemIndex := 0;
end;

procedure TForm1.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  ImgHolder.Picture.Assign(Image);
  Cancel := True;//Only want one image
end;

end.
