unit SimpleExampleForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DelphiTwain, DelphiTwain_VCL;

type
  TForm1 = class(TForm)
    Title: TPanel;
    ImageHolder: TImage;
    GoAcquire: TButton;
    procedure GoAcquireClick(Sender: TObject);
  private
    Twain: TDelphiTwain;

    procedure TwainTwainAcquire(Sender: TObject; const Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
  protected
    procedure DoDestroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DoDestroy;
begin
  Twain.Free;//Don't forget to free Twain!

  inherited;
end;

procedure TForm1.GoAcquireClick(Sender: TObject);
begin
  //Create Twain
  if Twain = nil then begin
    Twain := TDelphiTwain.Create;
    Twain.OnTwainAcquire := TwainTwainAcquire;
  end;

  //Load Twain Library dynamically
  if Twain.LoadLibrary then
  begin
    //Load source manager
    Twain.SourceManagerLoaded := TRUE;

    //Allow user to select source -> only the first time
    if not Assigned(Twain.SelectedSource) then
      Twain.SelectSource;

    if Assigned(Twain.SelectedSource) then begin
      //Load source, select transference method and enable (display interface)}
      Twain.SelectedSource.Loaded := TRUE;
      Twain.SelectedSource.ShowUI := TRUE;//display interface
      Twain.SelectedSource.Enabled := True;
    end;

  end else begin
    ShowMessage('Twain is not installed.');
  end;
end;

procedure TForm1.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  ImageHolder.Picture.Assign(Image);
  Cancel := True;//Only want one image
end;

end.
