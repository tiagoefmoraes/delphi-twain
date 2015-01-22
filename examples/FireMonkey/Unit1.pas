unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DelphiTwain, DelphiTwain_FMX,
  FMX.Objects, FMX.StdCtrls, FMX.Graphics;

type
  TForm1 = class(TForm)
    BtnAcquire: TButton;
    Image1: TImage;
    procedure BtnAcquireClick(Sender: TObject);
  private
    Twain: TDelphiTwain;

    procedure TwainTwainAcquire(Sender: TObject; const Index: Integer;
      Image: TBitmap; var Cancel: Boolean); overload;
  public
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.BtnAcquireClick(Sender: TObject);
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

destructor TForm1.Destroy;
begin
  Twain.Free;//Don't forget to free Twain!

  inherited;
end;

procedure TForm1.TwainTwainAcquire(Sender: TObject; const Index: Integer;
  Image: TBitmap; var Cancel: Boolean);
begin
  Image1.Bitmap.Assign(Image);
  Cancel := True;//Only want one image
end;

end.
