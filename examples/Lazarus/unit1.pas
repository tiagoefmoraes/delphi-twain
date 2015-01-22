unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DelphiTwain, DelphiTwain_VCL;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageHolder: TImage;
    procedure Button1Click(Sender: TObject);
  private
    Twain: TDelphiTwain;

    procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  //Create Twain
  if Twain = nil then begin
    Twain := TDelphiTwain.Create;
    Twain.OnTwainAcquire := @TwainTwainAcquire;
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
  ImageHolder.Picture.Bitmap.Assign(Image);
  Cancel := True;//Only want one image
end;

end.

