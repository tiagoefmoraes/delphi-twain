{DELPHI IMPLEMENTATION OF TWAIN INTERFACE}
{Initially created by Gustavo Daud in December 2003}

{This is my newest contribution for Delphi comunity, a powerfull}
{implementation of latest Twain features. As you know, twain is }
{the most common library to acquire images from most acquisition}
{devices such as Scanners and Web-Cameras.}

{Twain library is a bit different from other libraries, because}
{most of the hard work can be done by a a single method. Also it}
{automatically changes in the application message loop, which is}
{not a simple task, at least in delphi VCL.}

{It is not 100% sure to to Twain not to be installed in Windows,}
{as it ships with Windows and later and with most of the }
{acquisition device drivers (automatically with their installation)}
{This library dynamically calls the library, avoiding the application}
{hang when it is not present.}

{Also, as in most of my other components, I included a trigger}
{to allow the component to work without the heavy delphi VCL}
{for small final executables. To enable, edit DelphiTwain.inc}


{
CHANGE LOG:

2014/04/29 - Fix for unloading library cancelling acquire window on Lazarus
             Typo fixes in language constants; cosmetic fixes.
             (Thanks to Reinier).

2013/12/18 - FireMonkey support, color bug fix.

2013/08/18 - New method OnTransferComplete: fired when all documents are
  scanned or the scan is canceled. Thanks to Andrei Galatyn.

2013/07/26 - Color problems solved (thanks to Marco & Christian).
  TWAIN drivers did not respond - now both WIA and TWAIN can be used.

2012/11/01 - Ondrej Pokorny: small changes for Lazarus and 64bit compiler

2009/11/10 - Some changes to make it work in Delphi 2009, and above

2004/01/20 - Some updates and bug fixes by Nemeth Peter
}

unit DelphiTwain;

{$I DelphiTwain.inc}

interface

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

{Used units}
uses
  SysUtils, Windows, Messages,
  {$IFDEF FPC}Classes, {$ENDIF}
  Twain, DelphiTwainUtils;

const
  {Name of the Twain library for 32 bits enviroment}
  {$IFDEF WIN64}
  TWAINLIBRARY: String = 'TWAINDSM.DLL';
  {$ELSE}
  TWAINLIBRARY: String = 'TWAIN_32.DLL';
  {$ENDIF}

const
  {Error codes}
  ERROR_BASE              = 300;
  ERROR_INT16: TW_INT16   = HIGH(TW_INT16);

type
  {From twain}
  TW_STR255 = Twain.TW_STR255;

  {Forward declaration}
  TCustomDelphiTwain = class;

  {Component kinds}
  TTwainComponent = TObject;

  {File formats}
  TTwainFormat = (tfTIFF, tfPict, tfBMP, tfXBM, tfJPEG, tfFPX,
    tfTIFFMulti, tfPNG, tfSPIFF, tfEXIF, tfUnknown);
          {Twain units}
  TTwainUnit = (tuInches, tuCentimeters, tuPicas, tuPoints, tuTwips,
    tuPixels, tuUnknown);
  TTwainUnitSet = set of TTwainUnit;
  {Twain pixel flavor}
  TTwainPixelFlavor = (tpfChocolate, tpfVanilla, tpfUnknown);
  TTwainPixelFlavorSet = set of TTwainPixelFlavor;
  {Orientation}
  TTwainOrientation = (torPortrait, torLandscape);
  {Paper size}
  TTwainPaperSize = (tpsA4, tpsA5, tpsB4, tpsB5, tpsB6, tpsUSLetter, tpsUSLegal);
  {Auto size}
  TTwainAutoSize = (tasNone, tasAuto, tasCurrent);
  {Twain pixel type}
  TTwainPixelType = (tbdBw, tbdGray, tbdRgb, tbdPalette, tbdCmy, tbdCmyk,
    tbdYuv, tbdYuvk, tbdCieXYZ, tbdUnknown, tbdUnknown1, tbdUnknown2, tbdBgr);
  TTwainPixelTypeSet = set of TTwainPixelType;
  {Twain bit depth}
  TTwainBitDepth = array of TW_UINT16;
  {Twain resolutions}
  TTwainResolution = array of Extended;

  {Events}
  TOnTwainError = procedure(Sender: TObject; const Index: Integer; ErrorCode,
    Additional: Integer) of object;
  TOnSourceNotify = procedure(Sender: TObject; const Index: Integer) of object;
  TOnTransferComplete = procedure(Sender: TObject; const Index: Integer; const Canceled: Boolean) of object;
  TOnSourceFileTransfer = procedure(Sender: TObject; const Index: Integer;
    Filename: TW_STR255; Format: TTwainFormat; var Cancel: Boolean) of object;

  {Available twain languages}
  TTwainLanguage = ({-1}tlUserLocale = -1, tlDanish, tlDutch, tlInternationalEnglish,
    tlFrenchCanadian, tlFinnish, tlFrench, tlGerman, tlIcelandic, tlItalian,
    tlNorwegian, tlPortuguese, tlSpanish, tlSwedish, tlUsEnglish,
    tlAfrikaans, tlAlbania, tlArabic, tlArabicAlgeria, tlArabicBahrain, {18}
    tlArabicEgypt, tlArabicIraq, tlArabJordan, tlArabicKuwait,
    tlArabicLebanon, tlArabicLibya, tlArabicMorocco, tlArabicOman,
    tlArabicQatar, tlArabicSaudiarabia, tlArabicSyria, tlArabicTunisia,
    tlArabicUae, tlArabicYemen, tlBasque, tlByelorussian, tlBulgarian,  {35}
    tlCatalan, tlChinese, tlChineseHongkong, tlChinesePeoplesRepublic,
    tlChineseSingapore, tlChineseSimplified, tlChineseTwain, {42}
    tlChineseTraditional, tlCroatia, tlCzech, tlDutchBelgian, {46}
    tlEnglishAustralian, tlEnglishCanadian, tlEnglishIreland,
    tlEnglishNewZealand, tlEnglishSouthAfrica, tlEnglishUk, {52}
    tlEstonian, tlFaeroese, tlFarsi, tlFrenchBelgian, tlFrenchLuxembourg, {57}
    tlFrenchSwiss, tlGermanAustrian, tlGermanLuxembourg, tlGermanLiechtenstein,
    tlGermanSwiss, tlGreek, tlHebrew, tlHungarian, tlIndonesian, {66}
    tlItalianSwiss, tlJapanese, tlKorean, tlKoreanJohab, tlLatvian, {71}
    tlLithuanian, tlNorewgianBokmal, tlNorwegianNynorsk, tlPolish, {75}
    tlPortugueseBrazil, tlRomanian, tlRussian, tlSerbianLatin,
    tlSlovak, tlSlovenian, tlSpanishMexican, tlSpanishModern, tlThai,
    tlTurkish, tlUkranian, tlAssamese, tlBengali, tlBihari, tlBodo,
    tlDogri, tlGujarati {92}, tlHarayanvi, tlHindi, tlKannada, tlKashmiri,
    tlMalayalam, tlMarathi, tlMarwari, tlMeghalayan, tlMizo, tlNaga {102},
    tlOrissi, tlPunjabi, tlPushtu, tlSerbianCyrillic, tlSikkimi,
    tlSwedishFinland, tlTamil, tlTelugu, tlTripuri, tlUrdu, tlVietnamese);
  {Twain supported groups}
  TTwainGroups = set of (tgControl, tgImage, tgAudio);

  {Transfer mode for twain}
  TTwainTransferMode = (ttmFile, ttmNative, ttmMemory);

  {rect for LAYOUT; npeter 2004.01.12.}
  TTwainRect =
   record
    Left:   double;
    Top:    double;
    Right:  double;
    Bottom: double;
   end;

  {Object to handle TW_IDENTITY}
  TTwainIdentity = class(TObject)
  private
    {Sets application language property}
    procedure SetLanguage(const Value: TTwainLanguage);
    {Sets text values}
    procedure SetString(const Index: Integer; const Value: String);
    {Sets avaliable groups}
    procedure SetGroups(const Value: TTwainGroups);
  protected
    function GetCountryCode: Word;
    function GetMajorVersion: TW_UINT16;
    function GetMinorVersion: TW_UINT16;
    procedure SetCountryCode(const aCountryCode: Word);
    procedure SetMajorVersion(const aMajorVersion: TW_UINT16);
    procedure SetMinorVersion(const aMinorVersion: TW_UINT16);
  protected
    {Structure which should be filled}
    Structure: TW_IDENTITY;
    {Returns application language property}
    function GetLanguage(): TTwainLanguage;
    {Returns text values}
    function GetString(const Index: integer): String;
    {Returns avaliable groups}
    function GetGroups(): TTwainGroups;
  public
    {Object being created}
    constructor Create;
    {Copy properties from another TTwainIdentity}
    procedure Assign(Source: TObject);
  public
    {Application major version}
    property MajorVersion: TW_UINT16 read GetMajorVersion write SetMajorVersion;
    {Application minor version}
    property MinorVersion: TW_UINT16 read GetMinorVersion write SetMinorVersion;
    {Language}
    property Language: TTwainLanguage read GetLanguage write SetLanguage;
    {Country code}
    property CountryCode: Word read GetCountryCode write SetCountryCode;
    {Supported groups}
    property Groups: TTwainGroups read GetGroups write SetGroups;
    {Text values}
    property VersionInfo: String index 0 read GetString write
      SetString;
    {Scanner manufacturer}
    property Manufacturer: String index 1 read GetString write
      SetString;
    {Scanner product family}
    property ProductFamily: String index 2 read GetString write
      SetString;
    {Scanner product name}
    property ProductName: String index 3 read GetString write
      SetString;
  end;

  {Return set for capability retrieving/setting}
  TCapabilityRet = (crSuccess, crUnsupported, crBadOperation, crDependencyError,
    crLowMemory, crInvalidState, crInvalidContainer);
  {Kinds of capability retrieving}
  TRetrieveCap = (rcGet, rcGetCurrent, rcGetDefault, rcReset);
  {Capability list type}
  TGetCapabilityList = array of string;
  TSetCapabilityList = array of pointer;

  {Source object}
  TTwainSource = class(TTwainIdentity)
  private
    {Holds the item index}
    fIndex: Integer;
    {Transfer mode for the images}
    fTransferMode: TTwainTransferMode;
    {Stores if user interface should be shown}
    fShowUI: Boolean;
    {Stores if the source window is modal}
    fModal: Boolean;
    {Stores if the source is enabled}
    fEnabled: Boolean;
    {Stores if the source is loaded}
    fLoaded: Boolean;
    {Stores the owner}
    fOwner: TCustomDelphiTwain;
    {Used with property SourceManagerLoaded to test if the source manager}
    {is loaded or not.}
    function GetSourceManagerLoaded(): Boolean;
    {Returns a pointer to the application}
    function GetAppInfo(): pTW_IDENTITY;
    {Sets if the source is loaded}
    procedure SetLoaded(const Value: Boolean);
    {Sets if the source is enabled}
    procedure SetEnabled(const Value: Boolean);
    {Returns a pointer to the source pTW_IDENTITY}
    function GetStructure: pTW_IDENTITY;
    {Returns a resolution}
    function GetResolution(Capability: TW_UINT16; var Return: Extended;
      var Values: TTwainResolution; Mode: TRetrieveCap): TCapabilityRet;
  protected
    {Reads a native image}
    procedure ReadNative(Handle: TW_UINT32; var Cancel: Boolean);
    {Reads the file image}
    procedure ReadFile(Name: TW_STR255; Format: TW_UINT16; var Cancel: Boolean);
    {Call event for memory image}
    procedure ReadMemory(Image: HBitmap; var Cancel: Boolean);
  protected
    {Prepare image memory transference}
    function PrepareMemXfer(var BitmapHandle: HBitmap;
      var PixelType: TW_INT16): TW_UINT16;
    {Transfer image memory}
    function TransferImageMemory(var ImageHandle: HBitmap;
      {%H-}PixelType: TW_INT16): TW_UINT16;
    {Returns a pointer to the TW_IDENTITY for the application}
    property AppInfo: pTW_IDENTITY read GetAppInfo;
    {Method to transfer the images}
    procedure TransferImages();
    {Returns if the source manager is loaded}
    property SourceManagerLoaded: Boolean read GetSourceManagerLoaded;
    {Source configuration methods}
    {************************}
  protected
    {Gets an item and returns it in a string}
    procedure GetItem(var Return: String; ItemType: TW_UINT16; Data: Pointer);
    {Converts from a result to a TCapabilityRec}
    function ResultToCapabilityRec(const Value: TW_UINT16): TCapabilityRet;
    {Sets a capability}
    function SetCapabilityRec(const Capability, ConType: TW_UINT16;
      Data: HGLOBAL): TCapabilityRet;
  public
    {Message received in the event loop}
    function ProcessMessage(const Msg: TMsg): Boolean;
    {Returns a capability strucutre}
    function GetCapabilityRec(const Capability: TW_UINT16;
      var Handle: HGLOBAL; Mode: TRetrieveCap;
      var Container: TW_UINT16): TCapabilityRet;
    {************************}
    {Returns an one value capability}
    function GetOneValue(Capability: TW_UINT16;
      var ItemType: TW_UINT16; var Value: string;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF};
      MemHandle: HGLOBAL{$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet;
    {Returns an range capability}
    function GetRangeValue(Capability: TW_UINT16; var ItemType: TW_UINT16;
      var Min, Max, Step, Default, Current: String;
      MemHandle: HGLOBAL{$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet;
    {Returns an enumeration capability}
    function GetEnumerationValue(Capability: TW_UINT16;
      var ItemType: TW_UINT16; var List: TGetCapabilityList; var Current,
      Default: Integer; Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF};
      MemHandle: HGLOBAL{$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet;
    {Returns an array capability}
    function GetArrayValue(Capability: TW_UINT16; var ItemType: TW_UINT16;
      var List: TGetCapabilityList; MemHandle: HGLOBAL
      {$IFDEF DEFAULTPARAM}=0{$ENDIF}): TCapabilityRet;
    {************************}
    {Sets an one value capability}
    function SetOneValue(Capability: TW_UINT16; ItemType: TW_UINT16;
      Value: Pointer): TCapabilityRet;
    {Sets a range capability}
    function SetRangeValue(Capability, ItemType: TW_UINT16; Min, Max, Step,
      Current: TW_UINT32): TCapabilityRet;
    {Sets an enumeration capability}
    function SetEnumerationValue(Capability, ItemType: TW_UINT16;
      CurrentIndex: TW_UINT32; List: TSetCapabilityList): TCapabilityRet;
    {Sets an array capability}
    function SetArrayValue(Capability, ItemType: TW_UINT16;
      List: TSetCapabilityList): TCapabilityRet;
  public
    {Setup file transfer}
    function SetupFileTransfer(Filename: String; Format: TTwainFormat): Boolean;
  protected
    {Used with property PendingXfers}
    function GetPendingXfers(): TW_INT16;
  public
    {Set source transfer mode}
    //function ChangeTransferMode(NewMode: TTwainTransferMode): TCapabilityRet;
    {Transfer mode for transfering images from the source to}
    {the component and finally to the application}
    property TransferMode: TTwainTransferMode read fTransferMode write fTransferMode;
  public
    {Returns return status information}
    function GetReturnStatus(): TW_UINT16;
    {Capability setting}
    {Set the number of images that the application wants to receive}
    function SetCapXferCount(Value: SmallInt): TCapabilityRet;
    {Returns the number of images that the source will return}
    function GetCapXferCount(var Return: SmallInt;
      Mode: TRetrieveCap{$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Retrieve the unit measure for all quantities}
    function GetICapUnits(var Return: TTwainUnit;
      var Supported: TTwainUnitSet; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Set the unit measure}
    function SetICapUnits(Value: TTwainUnit): TCapabilityRet;
    {npeter 2004.01.12 begin}
    function SetImagelayoutFrame(const fLeft,fTop,fRight,
      fBottom: double): TCapabilityRet;
    function SetIndicators(Value: boolean): TCapabilityRet;
    {npeter 2004.01.12 end}
    {Retrieve the pixel flavor values}
    function GetIPixelFlavor(var Return: TTwainPixelFlavor;
      var Supported: TTwainPixelFlavorSet; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Set the pixel flavor values}
    function SetIPixelFlavor(Value: TTwainPixelFlavor): TCapabilityRet;
    {Set orientation}
    function SetOrientation(Value: TTwainOrientation): TCapabilityRet;
    {Set paper size}
    function SetPaperSize(Value: TTwainPaperSize): TCapabilityRet;
    {Set auto size}
    function SetAutoSize(Value: TTwainAutoSize): TCapabilityRet;
    {Set auto border detection}
    function SetAutoBorderDetection(Value: Boolean): TCapabilityRet;
    {Set auto rotate}
    function SetAutoRotate(Value: Boolean): TCapabilityRet;
    {Set auto Deskew}
    function SetAutoDeskew(Value: Boolean): TCapabilityRet;
    {Set undefined image size}
    function SetUndefinedImageSize(Value: Boolean): TCapabilityRet;
    {Returns bitdepth values}
    function GetIBitDepth(var Return: Word;
      var Supported: TTwainBitDepth; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Set current bitdepth value}
    function SetIBitDepth(Value: Word): TCapabilityRet;
    {Returns pixel type values}
    function GetIPixelType(var Return: TTwainPixelType;
      var Supported: TTwainPixelTypeSet; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Set the pixel type value}
    function SetIPixelType(Value: TTwainPixelType): TCapabilityRet;
    {Returns X and Y resolutions}
    function GetIXResolution(var Return: Extended; var Values: TTwainResolution;
      Mode: TRetrieveCap {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    function GetIYResolution(var Return: Extended; var Values: TTwainResolution;
      Mode: TRetrieveCap {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Sets X and X resolutions}
    function SetIXResolution(Value: Extended): TCapabilityRet;
    function SetIYResolution(Value: Extended): TCapabilityRet;
    {Returns physical width and height}
    function GetIPhysicalWidth(var Return: Extended; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    function GetIPhysicalHeight(var Return: Extended; Mode: TRetrieveCap
      {$IFDEF DEFAULTPARAM}=rcGet{$ENDIF}): TCapabilityRet;
    {Returns if user interface is controllable}
    function GetUIControllable(var Return: Boolean): TCapabilityRet;
    {Returns feeder is loaded or not}
    function GetFeederLoaded(var Return: Boolean): TCapabilityRet;
    {Returns/sets if feeder is enabled}
    function GetFeederEnabled(var Return: Boolean): TCapabilityRet;
    function SetFeederEnabled(Value: WordBool): TCapabilityRet;
    {Returns/sets if auto feed is enabled}
    function GetAutofeed(var Return: Boolean): TCapabilityRet;
    function SetAutoFeed(Value: WordBool): TCapabilityRet;
    {Returns number of pending transfer}
    property PendingXfers: TW_INT16 read GetPendingXfers;
  public
    {Enables the source}
    function EnableSource(ShowUI, Modal: Boolean): Boolean;
    {Disables the source}
    function DisableSource: Boolean;
    {Loads the source}
    function LoadSource(): Boolean;
    {Unloads the source}
    function UnloadSource(): Boolean;
    {Returns a pointer to the source identity}
    property SourceIdentity: pTW_IDENTITY read GetStructure;
    {Returns/sets if the source is enabled}
    property Enabled: Boolean read fEnabled write SetEnabled;
    {Returns/sets if this source is loaded}
    property Loaded: Boolean read fLoaded write SetLoaded;
    {Object being created/destroyed}
    constructor Create(AOwner: TCustomDelphiTwain);
    destructor Destroy; override;
    {Returns owner}
    property Owner: TCustomDelphiTwain read fOwner;
    {Source window is modal}
    property Modal: Boolean read fModal write fModal;
    {Sets if user interface should be shown}
    property ShowUI: Boolean read fShowUI write fShowUI;
    {Returns the item index}
    property Index: Integer read fIndex;
    {Convert properties from write/read to read only}
    {(read description on TTwainIdentity source)}
    property MajorVersion: TW_UINT16 read GetMajorVersion;
    property MinorVersion: TW_UINT16 read GetMinorVersion;
    property Language: TTwainLanguage read GetLanguage;
    property CountryCode: Word read GetCountryCode;
    property Groups: TTwainGroups read GetGroups;
    property VersionInfo: String index 0 read GetString;
    property Manufacturer: String index 1 read GetString;
    property ProductFamily: String index 2 read GetString;
    property ProductName: String index 3 read GetString;
  end;

  {Component part}
  TCustomDelphiTwain = class(TTwainComponent)
  private
    {Should contain the number of Twain sources loaded}
    fSourcesLoaded: Integer;
  private
    {Event pointer holders}
    fOnSourceDisable: TOnSourceNotify;
    fOnAcquireCancel: TOnSourceNotify;
    fOnSourceSetupFileXfer: TOnSourceNotify;
    fOnSourceFileTransfer: TOnSourceFileTransfer;
    fOnAcquireError: TOnTwainError;
    fOnTransferComplete: TOnTransferComplete;
  private
    fSelectedSourceIndex: Integer;
    {Temp variable to allow SourceCount to be displayed in delphi}
    {property editor}
    fDummySourceCount: Integer;
    {Contains list of source devices}
    DeviceList: TPointerList;
    {Contains a pointer to the structure with the application}
    {information}
    AppInfo: pTW_IDENTITY;
    {Holds the object to allow the user to set the application information}
    fInfo: TTwainIdentity;
    {Holds the handle for the virtual window which will receive}
    {twain message notifications}
    {Will hold Twain library handle}
    fHandle: HInst;
    {Holds if the component has enumerated the devices}
    fHasEnumerated: Boolean;
    {Holds twain dll procedure handle}
    fTwainProc: TDSMEntryProc;
    {Holds the transfer mode to be used}
    fTransferMode: TTwainTransferMode;
    {Contains if the library is loaded}
    fLibraryLoaded: Boolean;
    {Contains if the source manager was loaded}
    fSourceManagerLoaded: Boolean;
    {Set to true if the host application does not create any windows}
    fIsConsoleApplication: Boolean;
    {Procedure to load and unload twain library and update property}
    procedure SetLibraryLoaded(const Value: Boolean);
    {Procedure to load or unloaded the twain source manager}
    procedure SetSourceManagerLoaded(const Value: Boolean);
    {Updates the application information object}
    procedure SetInfo(const Value: TTwainIdentity);
    {Returns the number of sources}
    function GetSourceCount(): Integer;
    {Returns a source from the list}
    function GetSource(Index: Integer): TTwainSource;
    {Finds a matching source index}
    function FindSource(Value: pTW_IDENTITY): Integer;
    //Gets selected source
    function GetSelectedSource: TTwainSource;
    //Gets selected source index
    function GetSelectedSourceIndex: Integer;
    //Sets selected source index
    procedure SetSelectedSourceIndex(const Value: Integer);
    //Refresh the VirtualWindow - usually needed when transfer was completed
    procedure RefreshVirtualWindow;
  protected
    fVirtualWindow: THandle;

    {Returns the default source}
    function GetDefaultSource: Integer;

    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure MessageTimer_Enable; virtual; abstract;
    procedure MessageTimer_Disable; virtual; abstract;
    function CustomSelectSource: Integer; virtual; abstract;
    function CustomGetParentWindow: TW_HANDLE; virtual; abstract;

    procedure DoTwainAcquire(Sender: TObject; const Index: Integer; Image:
      HBitmap; var Cancel: Boolean); virtual; abstract;
    procedure DoAcquireProgress(Sender: TObject; const Index: Integer;
      const Image: HBitmap; const Current, Total: Integer); virtual; abstract;
  public
    {Clears the list of sources}
    procedure ClearDeviceList();
  public
    {Allows Twain to display a dialog to let the user choose any source}
    {and returns the source index in the list}
    function SelectSource(): Integer;
    {Returns the number of loaded sources}
    property SourcesLoaded: Integer read fSourcesLoaded;
    {Enumerate the avaliable devices after Source Manager is loaded}
    function EnumerateDevices(): Boolean;
    {Object being created}
    constructor Create; virtual;
    {Object being destroyed}
    destructor Destroy; override;
    {Loads twain library and returns if it loaded sucessfully}
    function LoadLibrary(): Boolean;
    {Unloads twain and returns if it unloaded sucessfully}
    function UnloadLibrary(): Boolean;
    {Loads twain source manager}
    function LoadSourceManager(): Boolean;
    {Unloads the source manager}
    function UnloadSourceManager(forced: boolean): Boolean;
    {Returns the application TW_IDENTITY}
    property AppIdentity: pTW_IDENTITY read AppInfo;
    {Returns Twain library handle}
    property Handle: HInst read fHandle;
    {Returns virtual window that receives messages}
    property VirtualWindow: THandle read fVirtualWindow;
    {Returns a pointer to Twain only procedure}
    property TwainProc: TDSMEntryProc read fTwainProc;
    {Holds if the component has enumerated the devices}
    property HasEnumerated: Boolean read fHasEnumerated;
    {Returns a source}
    property Source[Index: Integer]: TTwainSource read GetSource;
    {Set to true if the host application does not create any windows}
    property IsConsoleApplication: Boolean read fIsConsoleApplication write fIsConsoleApplication default False;
  public
    {Events}
    {Source being disabled}
    property OnSourceDisable: TOnSourceNotify read fOnSourceDisable
      write fOnSourceDisable;
    {Acquire cancelled}
    property OnAcquireCancel: TOnSourceNotify read fOnAcquireCancel
      write fOnAcquireCancel;
    {User should set information to prepare for the file transfer}
    property OnSourceSetupFileXfer: TOnSourceNotify read fOnSourceSetupFileXfer
      write fOnSourceSetupFileXfer;
    {File transfered}
    property OnSourceFileTransfer: TOnSourceFileTransfer read
      fOnSourceFileTransfer write fOnSourceFileTransfer;
    {Acquire error}
    property OnAcquireError: TOnTwainError read fOnAcquireError
      write fOnAcquireError;
    {All images transfered}
    property OnTransferComplete: TOnTransferComplete read fOnTransferComplete
      write fOnTransferComplete;
  public
    {Default transfer mode to be used with sources}
    property TransferMode: TTwainTransferMode read fTransferMode
      write fTransferMode;
    {Returns the number of sources, after Library and Source Manager}
    {has being loaded}
    property SourceCount: Integer read GetSourceCount write fDummySourceCount;
    //Selected source in a dialog
    property SelectedSourceIndex: Integer read GetSelectedSourceIndex write SetSelectedSourceIndex;
    //Selected source in a dialog
    property SelectedSource: TTwainSource read GetSelectedSource;
    {User should fill the application information}
    property Info: TTwainIdentity read fInfo write SetInfo;
    {Loads or unload Twain library}
    property LibraryLoaded: Boolean read fLibraryLoaded write SetLibraryLoaded;
    {Loads or unloads the source manager}
    property SourceManagerLoaded: Boolean read fSourceManagerLoaded write
      SetSourceManagerLoaded;
  end;

{Puts a string inside a TW_STR255}
{$IFDEF UNICODE}
function StrToStr255(Value: RawByteString): TW_STR255;
{$ELSE}
function StrToStr255(Value: String): TW_STR255;
{$ENDIF}
{This method returns if Twain is installed in the current machine}
function IsTwainInstalled(): Boolean;
{Called by Delphi to register the component}
{Returns the size of a twain type}
function TWTypeSize(TypeName: TW_UINT16): Integer;

function MakeMsg(const Handle: THandle; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): TMsg;

implementation

{Returns the size of a twain type}
function TWTypeSize(TypeName: TW_UINT16): Integer;
begin
  {Test the type to return the size}
  case TypeName of
    TWTY_INT8  :  Result := sizeof(TW_INT8);
    TWTY_UINT8 :  Result := sizeof(TW_UINT8);
    TWTY_INT16 :  Result := sizeof(TW_INT16);
    TWTY_UINT16:  Result := sizeof(TW_UINT16);
    TWTY_INT32 :  Result := sizeof(TW_INT32);
    TWTY_UINT32:  Result := sizeof(TW_UINT32);
    TWTY_FIX32 :  Result := sizeof(TW_FIX32);
    TWTY_FRAME :  Result := sizeof(TW_FRAME);
    TWTY_STR32 :  Result := sizeof(TW_STR32);
    TWTY_STR64 :  Result := sizeof(TW_STR64);
    TWTY_STR128:  Result := sizeof(TW_STR128);
    TWTY_STR255:  Result := sizeof(TW_STR255);
    //npeter: the following types were not implemented
    //especially the bool caused problems
    TWTY_BOOL:    Result := sizeof(TW_BOOL);
    TWTY_UNI512:  Result := sizeof(TW_UNI512);
    TWTY_STR1024:  Result := sizeof(TW_STR1024);
    else          Result := 0;
  end {case}
end;

{Puts a string inside a TW_STR255}
{$IFDEF UNICODE}
function StrToStr255(Value: RawByteString): TW_STR255;
{$ELSE}
function StrToStr255(Value: String): TW_STR255;
{$ENDIF}
begin
  {Clean result}
  Fillchar({%H-}Result, sizeof(TW_STR255), #0);
  {If value fits inside the TW_STR255, copy memory}
  if Length(Value) <= sizeof(TW_STR255) then
    CopyMemory(@Result[0], @Value[1], Length(Value))
  else CopyMemory(@Result[0], @Value[1], sizeof(TW_STR255));
end;

{Returns full Twain directory (usually in Windows directory)}
function GetTwainDirectory(): String;
var
  i: TDirectoryKind;
  Dir: String;
begin
  {Searches in all the directories}
  FOR i := LOW(TDirectoryKind) TO HIGH(TDirectoryKind) DO
  begin

    {Directory to search}
    Dir := GetCustomDirectory(i);
    {Tests if the file exists in this directory}
    if FileExists(Dir + String(TWAINLIBRARY)) then
    begin
      {In case it exists, returns this directory and exit}
      {the for loop}
      Result := Dir;
      Break;
    end {if FileExists}

  end {FOR i}
end;

{This method returns if Twain is installed in the current machine}
function IsTwainInstalled(): Boolean;
begin
  {If GetTwainDirectory function returns an empty string, it means}
  {that Twain was not found}
  Result := (GetTwainDirectory() <> '');
end;

{ TTwainIdentity object implementation }

{Object being created}
constructor TTwainIdentity.Create;
begin
  {Allows ancestor to work}
  inherited Create;

  {Set initial properties}
  FillChar(Structure, sizeof(Structure), #0);
  Language := tlUserLocale;
  CountryCode := 1;
  MajorVersion := 1;
  VersionInfo := 'Application name';
  Structure.ProtocolMajor := TWON_PROTOCOLMAJOR;
  Structure.ProtocolMinor := TWON_PROTOCOLMINOR;
  Groups := [tgImage, tgControl];
  Manufacturer := 'Application manufacturer';
  ProductFamily := 'App product family';
  ProductName := 'App product name';
end;

{Sets a text value}
procedure TTwainIdentity.SetString(const Index: Integer;
  const Value: String);
var
  PropStr: PAnsiChar;
begin
  {Select and copy pointer}
  case Index of
    0: PropStr := @Structure.Version.Info[0];
    1: PropStr := @Structure.Manufacturer[0];
    2: PropStr := @Structure.ProductFamily[0];
  else PropStr := @Structure.ProductName[0];
  end {case};

  {Set value}
  Fillchar(PropStr^, sizeof(TW_STR32), #0);
  if Length(Value) > sizeof(TW_STR32) then
    CopyMemory(PropStr, @Value[1], sizeof(TW_STR32))
  else
    CopyMemory(PropStr, @Value[1], Length(Value));
end;

{Returns a text value}
function TTwainIdentity.GetString(const Index: Integer): String;
begin
  {Test for the required property}
  case Index of
    0: Result := string(Structure.Version.Info);
    1: Result := string(Structure.Manufacturer);
    2: Result := string(Structure.ProductFamily);
  else Result := string(Structure.ProductName);
  end {case}
end;

{Returns application language property}
function TTwainIdentity.GetLanguage(): TTwainLanguage;
begin
  Result := TTwainLanguage(Structure.Version.Language + 1);
end;

function TTwainIdentity.GetMajorVersion: TW_UINT16;
begin
  Result := Structure.Version.MajorNum;
end;

function TTwainIdentity.GetMinorVersion: TW_UINT16;
begin
  Result := Structure.Version.MinorNum;
end;

{Sets application language property}
procedure TTwainIdentity.SetLanguage(const Value: TTwainLanguage);
begin
  Structure.Version.Language := Word(Value) - 1;
end;

procedure TTwainIdentity.SetMajorVersion(const aMajorVersion: TW_UINT16);
begin
  Structure.Version.MajorNum := aMajorVersion;
end;

procedure TTwainIdentity.SetMinorVersion(const aMinorVersion: TW_UINT16);
begin
  Structure.Version.MinorNum := aMinorVersion;
end;

{Copy properties from another TTwainIdentity}
procedure TTwainIdentity.Assign(Source: TObject);
begin
  {The source should also be a TTwainIdentity}
  if Source is TTwainIdentity then begin
    {Copy properties}
    Structure := TTwainIdentity(Source).Structure
  end;
end;

{Returns avaliable groups}
function TTwainIdentity.GetCountryCode: Word;
begin
  Result := Structure.Version.Country;
end;

function TTwainIdentity.GetGroups(): TTwainGroups;
begin
  {Convert from Structure.SupportedGroups to TTwainGroups}
 Result := [];
 Include(Result, tgControl);
  if DG_IMAGE AND Structure.SupportedGroups <> 0 then
    Include(Result, tgImage);
  if DG_AUDIO AND Structure.SupportedGroups <> 0 then
    Include(Result, tgAudio);
end;

{Sets avaliable groups}
procedure TTwainIdentity.SetCountryCode(const aCountryCode: Word);
begin
  Structure.Version.Country := aCountryCode;
end;

procedure TTwainIdentity.SetGroups(const Value: TTwainGroups);
begin
  {Convert from TTwainGroups to Structure.SupportedGroups}
  Structure.SupportedGroups := DG_CONTROL;
  if tgImage in Value then
    Structure.SupportedGroups := Structure.SupportedGroups or DG_IMAGE;
  if tgAudio in Value then
    Structure.SupportedGroups := Structure.SupportedGroups or DG_AUDIO;
end;

{ TCustomDelphiTwain component implementation }

{Loads twain library and returns if it loaded sucessfully}
function TCustomDelphiTwain.LoadLibrary(): Boolean;
var
  TwainDirectory: String;
begin
  {The library must not be already loaded}
  if (not LibraryLoaded) then
  begin
    Result := FALSE; {Initially returns FALSE}
    {Searches for Twain directory}
    TwainDirectory := GetTwainDirectory();
    {Continue only if twain is installed in an known directory}
    if TwainDirectory <> '' then
    begin

      fHandle := Windows.LoadLibrary(PChar(TwainDirectory + TWAINLIBRARY));
      {If the library was sucessfully loaded}
      if (fHandle <> INVALID_HANDLE_VALUE) then
      begin

        {Obtains method handle}
        @fTwainProc := GetProcAddress(fHandle, MAKEINTRESOURCE(1));
        {Returns TRUE/FALSE if the method was obtained}
        Result := (@fTwainProc <> nil);

        {If the method was not obtained, also free the library}
        if not Result then
        begin
          {Free the handle and clears the variable}
          Windows.FreeLibrary(fHandle);
          fHandle := 0;
        end {if not Result}
      end
      else
        {If it was not loaded, clears handle value}
        fHandle := 0;

    end {if TwainDirectory <> ''};

  end
  else
    {If it was already loaded, returns true, since that is}
    {what was supposed to happen}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  if Result then fLibraryLoaded := TRUE;
end;


{Unloads twain and returns if it unloaded sucessfully}
function TCustomDelphiTwain.UnloadLibrary(): Boolean;
begin
  {The library must not be already unloaded}
  if (LibraryLoaded) then
  begin
    {Unloads the source manager}
    SourceManagerLoaded := FALSE;
    {Just call windows method to unload}
    Result := Windows.FreeLibrary(Handle);
    {If it was sucessfull, also clears handle value}
    if Result then fHandle := 0;
    {Updates property}
    fLibraryLoaded := not Result;
  end
  else
    {If it was already unloaded, returns true, since that is}
    {what was supposed to happen}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  {if Result then }fLibraryLoaded := FALSE;
  MessageTimer_Disable;
end;

{Enumerate the avaliable devices after Source Manager is loaded}
function TCustomDelphiTwain.EnumerateDevices(): Boolean;
var
  NewSource: TTwainSource;
  CallRes  : TW_UINT16;
begin
  {Booth library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    {Clears the preview list of sources}
    ClearDeviceList();

    {Allocate new identity and tries to enumerate}
    NewSource := TTwainSource.Create(Self);
    CallRes := TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
      MSG_GETFIRST, @NewSource.Structure);
    if CallRes = TWRC_SUCCESS then
      repeat

        {Add this item to the list}
        DeviceList.Add(NewSource);
        {Allocate memory for the next}
        NewSource := TTwainSource.Create(Self);
        NewSource.TransferMode := Self.TransferMode;
        NewSource.fIndex := DeviceList.Count;

      {Try to get the next item}
      until TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
        MSG_GETNEXT, @NewSource.Structure) <> TWRC_SUCCESS;

    {Set that the component has enumerated the devices}
    {if everything went correctly}
    Result := TRUE;
    fHasEnumerated := Result;

    {Dispose un-needed source object}
    NewSource.Free;

  end
  else Result := FALSE; {If library and source manager aren't loaded}
end;

{Procedure to load and unload twain library and update property}
procedure TCustomDelphiTwain.SetLibraryLoaded(const Value: Boolean);
begin
  {The value must be changing to activate}
  if (Value <> fLibraryLoaded) then
  begin
    {Depending on the parameter load/unload the library and updates}
    {property whenever it loaded or unloaded sucessfully}
    if Value           then  LoadLibrary()
    else {if not Value then} UnloadLibrary();

  end {if (Value <> fLibraryLoaded)}
end;

{Loads twain source manager}
function TCustomDelphiTwain.LoadSourceManager(): Boolean;
begin
  {The library must be loaded}
  if LibraryLoaded and not SourceManagerLoaded then begin
    {Loads source manager}
    Result := (fTwainProc(AppInfo, nil, DG_CONTROL, DAT_PARENT,
      MSG_OPENDSM, @VirtualWindow) = TWRC_SUCCESS);
  end else begin
    {The library is not loaded, thus the source manager could}
    {not be loaded}
    Result := FALSE or SourceManagerLoaded;
  end;

  {In case the method was sucessful, updates property}
  if Result then fSourceManagerLoaded := TRUE;
end;

procedure TCustomDelphiTwain.RefreshVirtualWindow;
begin
  //BUG WORKAROUND
  DoDestroy;
  DoCreate;

  if LoadLibrary then
    SourceManagerLoaded := True;
end;

{UnLoads twain source manager}
function TCustomDelphiTwain.UnloadSourceManager(forced: boolean): Boolean;
begin
  {The library must be loaded}
  if LibraryLoaded and SourceManagerLoaded then
  begin
    {Clears the list of sources}
    ClearDeviceList();
    {Unload source manager}
    if not forced then
     Result := (TwainProc(AppInfo, nil, DG_CONTROL, DAT_PARENT, MSG_CLOSEDSM, @VirtualWindow) = TWRC_SUCCESS)
    else result:=true;
  end
  else
    {The library is not loaded, meaning that the Source Manager isn't either}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  if Result then fSourceManagerLoaded := FALSE;
end;

procedure TCustomDelphiTwain.DoCreate;
begin
  {Create source list}
  DeviceList := TPointerList.Create;
  {Clear variables}
  fSourcesLoaded := 0;
  fHandle := 0;
  @fTwainProc := nil;
  fSourceManagerLoaded := FALSE;
  fHasEnumerated := FALSE;
  fTransferMode := ttmNative;

  {Creates the object to allow the user to set the application}
  {information to inform twain source manager and sources}
  fInfo := TTwainIdentity.Create;
  AppInfo := @fInfo.Structure;
end;

procedure TCustomDelphiTwain.DoDestroy;
begin
  {Completely unload the library}
  LibraryLoaded := FALSE;

  {Free the object}
  fInfo.Free;
  {Clears and free source list}
  ClearDeviceList();
  DeviceList.Free();
end;

{Returns a TMsg structure}
function MakeMsg(const Handle: THandle; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): TMsg;
begin
  {Fill structure with the parameters}
  Result.hwnd := Handle;
  Result.message := uMsg;
  Result.wParam := wParam;
  Result.lParam := lParam;
  GetCursorPos(Result.pt);
end;

{Procedure to load or unloaded the twain source manager}
procedure TCustomDelphiTwain.SetSelectedSourceIndex(const Value: Integer);
begin
  fSelectedSourceIndex := Value;
end;

procedure TCustomDelphiTwain.SetSourceManagerLoaded(const Value: Boolean);
begin
  {The library must be loaded to have access to the method}
  if LibraryLoaded and (Value <> fSourceManagerLoaded) then
  begin
    {Load/unload the source manager}
    if Value           then  LoadSourceManager()
    else {if not Value then} UnloadSourceManager(false);
  end {if LibraryLoaded}
end;

{Clears the list of sources}
procedure TCustomDelphiTwain.ClearDeviceList();
var
  i: Integer;
begin
  {Deallocate pTW_IDENTITY}
  FOR i := 0 TO DeviceList.Count - 1 DO
    TTwainSource(DeviceList.Item[i]).Free;
  {Clears the list}
  DeviceList.Clear;
  {Set trigger to tell that it has not enumerated again yet}
  fHasEnumerated := FALSE;

end;

{Finds a matching source index}
function TCustomDelphiTwain.FindSource(Value: pTW_IDENTITY): Integer;
var
  i       : Integer;
begin
  Result := -1; {Default result}

  {Search for this source in the list}
  for i := 0 TO SourceCount - 1 DO
    if CompareMem(@Source[i].Structure, PAnsiChar(Value), SizeOf(TW_IDENTITY)) then
    begin
      {Return index and exit}
      Result := i;
      break;
    end; {if CompareMem, for i}
end;

{Allows Twain to display a dialog to let the user choose any source}
{and returns the source index in the list}
function TCustomDelphiTwain.SelectSource: Integer;
begin
  Result := -1;     {Default result}
  {Booth library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    Result := CustomSelectSource;

    SelectedSourceIndex := Result;
  end {(LibraryLoaded and SourceManagerLoaded)}
end;

{Returns the number of sources}
function TCustomDelphiTwain.GetSourceCount(): Integer;
begin
  {Library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin
    {Enumerate devices, if needed}
    if not HasEnumerated then EnumerateDevices();
    {Returns}
    Result := DeviceList.Count;
  end
  {In case library and source manager aren't loaded, returns 0}
  else Result := 0
end;

{Returns the default source}
function TCustomDelphiTwain.GetDefaultSource: Integer;
var
  Identity: TW_IDENTITY;
begin
  {Call twain to display the dialog}
  if SourceManagerLoaded and (TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
    MSG_GETDEFAULT, @Identity) = TWRC_SUCCESS) then
    Result := FindSource(@Identity)
  else Result := 0 {Returns}
end;

{Returns a source from the list}
function TCustomDelphiTwain.GetSelectedSource: TTwainSource;
begin
  if SourceCount = 0 then begin
    Result := nil;
  end else begin
    if (fSelectedSourceIndex >= 0) and (fSelectedSourceIndex < SourceCount) then
      Result := Source[fSelectedSourceIndex]
    else
      Result := nil;
  end;
end;

function TCustomDelphiTwain.GetSelectedSourceIndex: Integer;
begin
  Result := fSelectedSourceIndex;
end;

function TCustomDelphiTwain.GetSource(Index: Integer): TTwainSource;
begin
  {Both library and source manager must be loaded}
  if (LibraryLoaded and SourceManagerLoaded) then
  begin

    {If index is in range, returns}
    {(Call to SourceCount property enumerates the devices, if needed)}
    if Index in [0..SourceCount - 1] then
      Result := DeviceList.Item[Index]
    else if (Index = -1) and (SourceCount > 0) then
      Result := DeviceList.Item[GetDefaultSource]
    {Unknown object, returns nil}
    else Result := nil;

  end
  {In case either the library or the source manager aren't}
  {loaded, it returns nil}
  else Result := nil
end;

{Object being created}
constructor TCustomDelphiTwain.Create;
begin
  inherited Create;

  fSelectedSourceIndex := -1;

  DoCreate;
end;

{Object being destroyed}
destructor TCustomDelphiTwain.Destroy;
begin
  DoDestroy;

  {Let ancestor class handle}
  inherited Destroy;
end;

{Updates the application information object}
procedure TCustomDelphiTwain.SetInfo(const Value: TTwainIdentity);
begin
  {Assign one object to another}
  fInfo.Assign(Value);
end;

{ TTwainSource object implementation }

{Used with property SourceManagerLoaded to test if the source manager}
{is loaded or not.}
function TTwainSource.GetSourceManagerLoaded: Boolean;
begin
  {Obtain information from owner TCustomDelphiTwain}
  Result := Owner.SourceManagerLoaded;
end;

{Sets if the source is loaded}
procedure TTwainSource.SetLoaded(const Value: Boolean);
begin
  {Value should be changing}
  if (Value <> fLoaded) then
  begin
    {Loads or unloads the source}
    if Value           then  LoadSource()
    else {if not Value then} UnloadSource();
  end {if (Value <> fLoaded)}
end;

{Sets if the source is enabled}
procedure TTwainSource.SetEnabled(const Value: Boolean);
begin
  {Source must be already enabled and value changing}
  if (Loaded) and (Value <> fEnabled) then
  begin
    {Enables/disables}
    if Value           then  EnableSource(ShowUI, Modal)
    else {if not Value then} DisableSource();
  end {if (Loaded) and (Value <> fEnabled)}
end;

{Enables the source}
function TTwainSource.EnableSource(ShowUI, Modal: Boolean): Boolean;
var
  twUserInterface: TW_USERINTERFACE;
begin
  {Source must be loaded and the value changing}
  if (Loaded) and (not Enabled) then
  begin
    {Builds UserInterface structure}
    twUserInterface.ShowUI := ShowUI;
    twUserInterface.ModalUI := Modal;
    twUserInterface.hParent := Owner.CustomGetParentWindow;

    //npeter may be it is better to send messages to VirtualWindow
    //I am not sure, but it seems more stable with a HP TWAIN driver
    //it was: := GetActiveWindow;
    //fEnabled := TRUE;
    Owner.MessageTimer_Enable;
    {Call method}
    Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
      DAT_USERINTERFACE, MSG_ENABLEDS, @twUserInterface) in
      [TWRC_SUCCESS, TWRC_CHECKSTATUS]);
  end
  else {If it's either not loaded or already enabled}
    {If it is not loaded}
    Result := FALSE or Enabled;

  {Updates property}
  if (Result = TRUE) then fEnabled := TRUE;
end;

{Disables the source}
function TTwainSource.DisableSource(): Boolean;
var
  twUserInterface: TW_USERINTERFACE;
begin
  {Source must be loaded and the value changing}
  if (Loaded) and (Enabled) then
  begin

    {Call method}
    Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
      DAT_USERINTERFACE, MSG_DISABLEDS, @twUserInterface) = TWRC_SUCCESS);
    {Call notification event if being used}
    if (Result) and (Assigned(Owner.OnSourceDisable)) then
      Owner.OnSourceDisable(Owner, Index);

  end
  else {If it's either not loaded or already disabled}
    {If it is not loaded}
    Result := TRUE;

  {Updates property}
  //if (Result = TRUE) then fEnabled := FALSE;
  fEnabled := False;
  Owner.MessageTimer_Disable;
end;

{Loads the source}
function TTwainSource.LoadSource: Boolean;
begin
  {Only loads if it is not already loaded}
  if Not Loaded then
  begin
    Result := (Owner.TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
      MSG_OPENDS, @Structure) = TWRC_SUCCESS);
    {Increase the loaded sources count variable}
    if Result then inc(Owner.fSourcesLoaded);
  end
  else
    {If it was already loaded, returns true}
    Result := TRUE;

  {In case the method was sucessful, updates property}
  if Result then
    fLoaded := TRUE;

end;

{Unloads the source}
function TTwainSource.UnloadSource: Boolean;
begin
  {Only unloads if it is loaded}
  if Loaded then
  begin
    {If the source was enabled, disable it}
    DisableSource();
    {Call method to load}
    Result := (Owner.TwainProc(AppInfo, nil, DG_CONTROL, DAT_IDENTITY,
      MSG_CLOSEDS, @Structure) = TWRC_SUCCESS);
    {Decrease the loaded sources count variable}
    if Result then dec(Owner.fSourcesLoaded);
  end
  else
    {If it was already unloaded, returns true}
    Result := TRUE;

  {In case the method was sucessful, updates property}
    fLoaded := FALSE;
end;

{Object being destroyed}
destructor TTwainSource.Destroy;
begin
  {If loaded, unloads source}
  UnloadSource();
  {Let ancestor class process}
  inherited Destroy;
end;

{Returns a pointer to the application}
function TTwainSource.GetAppInfo: pTW_IDENTITY;
begin
  Result := Owner.AppInfo;
end;

{Returns a pointer to the source identity}
function TTwainSource.GetStructure: pTW_IDENTITY;
begin
  Result := @Structure;
end;

{Object being created}
constructor TTwainSource.Create(AOwner: TCustomDelphiTwain);
begin
  {Allows ancestor class to process}
  inherited Create;

  {Initial values}
  fTransferMode := AOwner.TransferMode;
  fLoaded := FALSE;
  fShowUI := TRUE;
  fEnabled := FALSE;
  fModal := TRUE;
  {Stores owner}
  fOwner := AOwner;
end;

{Set source transfer mode}
{function TTwainSource.ChangeTransferMode(
  NewMode: TTwainTransferMode): TCapabilityRet;
const
  TransferModeToTwain: Array[TTwainTransferMode] of TW_UINT16 =
    (TWSX_FILE, TWSX_NATIVE, TWSX_MEMORY);
var
  Value: TW_UINT16;
begin
  //Set transfer mode method
  Value := TransferModeToTwain[NewMode];
  Result := SetOneValue(ICAP_XFERMECH, TWTY_UINT16, @Value);
  TransferMode := NewMode;
end;}

{Message received in the event loop}
function TTwainSource.ProcessMessage(const Msg: TMsg): Boolean;
var
  twEvent: TW_EVENT;
begin
  {Make twEvent structure}
  twEvent.TWMessage := MSG_NULL;
  twEvent.pEvent := TW_MEMREF(@Msg);
  {Call Twain procedure to handle message}
  Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_EVENT,
    MSG_PROCESSEVENT, @twEvent) = TWRC_DSEVENT);

  {If it is a message from the source, process}
  if Result then
    case twEvent.TWMessage of
      {No message from the source}
      MSG_NULL: exit;
      {Requested to close the source}
      MSG_CLOSEDSREQ:
      begin
        {Call notification event}
        if (Assigned(Owner.OnAcquireCancel)) then
          Owner.OnAcquireCancel(Owner, Index);
        if Assigned(Owner.OnTransferComplete) then
          Owner.OnTransferComplete(Owner, Index, True);
        {Disable the source}
        DisableSource();
        Owner.RefreshVirtualWindow;
      end;
      {Ready to transfer the images}
      MSG_XFERREADY:
        {Call method to transfer}
        TransferImages();

      MSG_CLOSEDSOK:
       result:=true;

      MSG_DEVICEEVENT:
       result:=true;

    end {case twEvent.TWMessage}
end;

{Returns return status information}
function TTwainSource.GetReturnStatus: TW_UINT16;
var
  StatusInfo: TW_STATUS;
begin
  {The source must be loaded in order to get the status}
  if Loaded then
  begin
    {Call method to get the information}
    Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_STATUS, MSG_GET,
      @StatusInfo);
    Result := StatusInfo.ConditionCode;
  end else Result := 0 {In case it was called while the source was not loaded}
end;

{Converts from a result to a TCapabilityRec}
function TTwainSource.ResultToCapabilityRec(
  const Value: TW_UINT16): TCapabilityRet;
begin

  {Test result code to return}
  case Value of
    {Successull, copy handle and return a success value}
    TWRC_SUCCESS: Result := crSuccess;
    {Error, get more on the error, and return result}
    {case} else
      case GetReturnStatus() of
         TWCC_CAPUNSUPPORTED: Result := crUnsupported;
         TWCC_CAPBADOPERATION: Result := crBadOperation;
         TWCC_CAPSEQERROR: Result := crDependencyError;
         TWCC_LOWMEMORY: Result := crLowMemory;
         TWCC_SEQERROR: Result := crInvalidState;
         else Result := crBadOperation;
      end {case GetReturnStatus of}
  end {case};

end;

{Sets a capability}
function TTwainSource.SetCapabilityRec(const Capability,
  ConType: TW_UINT16; Data: HGlobal): TCapabilityRet;
var
  CapabilityInfo: TW_CAPABILITY;
begin
  {Source must be loaded to set}
  if Loaded then
  begin

    {Fill structure}
    CapabilityInfo.Cap := Capability;
    CapabilityInfo.ConType := ConType;
    CapabilityInfo.hContainer := Data;

    {Call method and store return}
    Result := ResultToCapabilityRec(Owner.TwainProc(AppInfo, @Structure,
      DG_CONTROL, DAT_CAPABILITY, MSG_SET, @CapabilityInfo));

  end
  else Result := crInvalidState  {In case the source is not loaded}
end;

{Returns a capability strucutre}
function TTwainSource.GetCapabilityRec( const Capability: TW_UINT16;
  var Handle: HGLOBAL; Mode: TRetrieveCap;
  var Container: TW_UINT16): TCapabilityRet;
const
  ModeToTwain: Array[TRetrieveCap] of TW_UINT16 = (MSG_GET, MSG_GETCURRENT,
    MSG_GETDEFAULT, MSG_RESET);
var
  CapabilityInfo: TW_CAPABILITY;
begin
  {Source must be loaded}
  if Loaded then
  begin

    {Fill structure}
    CapabilityInfo.Cap := Capability;
    CapabilityInfo.ConType := TWON_DONTCARE16;
    CapabilityInfo.hContainer := 0;

    {Call method and store return}
    Result := ResultToCapabilityRec(Owner.TwainProc(AppInfo, @Structure,
      DG_CONTROL, DAT_CAPABILITY, ModeToTwain[Mode], @CapabilityInfo));

    if Result = crSuccess then
    begin
      Handle := CapabilityInfo.hContainer;
      Container := CapabilityInfo.ConType;
    end
  end {if not Loaded}
  else Result := crInvalidState  {In case the source is not loaded}
end;

{Gets an item and returns it in a string}
procedure TTwainSource.GetItem(var Return: String; ItemType: TW_UINT16;
  Data: Pointer);
begin
  {Test the item type}
  case ItemType of
    TWTY_INT8   :         Return := IntToStr(pTW_INT8(Data)^);
    TWTY_UINT8  :         Return := IntToStr(pTW_UINT8(Data)^);
    TWTY_INT16,
    44 {TWTY_HANDLE} :    Return := IntToStr(pTW_INT16(Data)^);
    TWTY_UINT16,
    TWTY_BOOL   :         Return := IntToStr(pTW_UINT16(Data)^);
    TWTY_INT32  :         Return := IntToStr(pTW_INT32(Data)^);
    TWTY_UINT32,
    43 {TWTY_MEMREF} :    Return := IntToStr(pTW_UINT32(Data)^);
    {Floating integer type}
    TWTY_FIX32:
      with pTW_FIX32(Data)^ do
        //npeter bugfix:
        //it is better to use the actual decimal separator
        //and not a wired in value!
        //If not, you may get error on strtofloat
        //original: Return := IntToStr(Whole) + ',' + IntToStr(Frac);
        Return := IntToStr(Whole) + {%H-}{$IFDEF DELPHI_XE2_UP}FormatSettings.{$ENDIF}DecimalSeparator + IntToStr(Frac);
    {String types, which are all ended by a null char (#0)}
    TWTY_STR32,
    TWTY_STR64,
    TWTY_STR128,
    TWTY_STR255 :         Return := String(PAnsiChar(Data));

  end {case ItemType}
end;

{Returns an array capability}
function TTwainSource.GetArrayValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var List: TGetCapabilityList;
  MemHandle: HGLOBAL): TCapabilityRet;
var
  ArrayV   : pTW_ARRAY;
  ItemSize : Integer;
  Data     : PAnsiChar;//ccc
  CurItem  : Integer;
  Value    : String;
  Container: TW_UINT16;
begin
  {Call method to get the memory to the return}
  if MemHandle = 0 then
    Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container)
  else
  begin
    Result := crSuccess;
    Container := TWON_ARRAY;
  end;

  if (Result = crSuccess) and (Container <> TWON_ARRAY) then
  begin
    Result := crInvalidContainer;
    GlobalFree(MemHandle);
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    ArrayV := GlobalLock(MemHandle);

    {Fill return properties}
    ItemType := ArrayV^.ItemType;

    {Prepare to list items}
    ItemSize := TWTypeSize(ItemType);
    Data := @ArrayV^.ItemList[0];
    SetLength(List, ArrayV^.NumItems);

    {Copy items}
    for CurItem := 0 TO ArrayV^.NumItems - 1 do
    begin
      {Obtain this item}
      GetItem({%H-}Value, ItemType, Data);
      List[CurItem] := Value;
      {Move memory to the next}
      inc(Data, ItemSize);
    end;


    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end {if (Result = crSuccess)}
end;

{Returns an enumeration capability}
function TTwainSource.GetEnumerationValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var List: TGetCapabilityList;
  var Current, Default: Integer; Mode: TRetrieveCap;
  MemHandle: HGLOBAL): TCapabilityRet;
var
  EnumV    : pTW_ENUMERATION;
  ItemSize : Integer;
  Data     : PAnsiChar;//ccc
  CurItem  : Integer;
  Value    : String;
  Container: TW_UINT16;
begin
  {Call method to get the memory to the return}
  if MemHandle = 0 then
    Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container)
  else
  begin
    Result := crSuccess;
    Container := TWON_ENUMERATION;
  end;

  if (Result = crSuccess) and (Container <> TWON_ENUMERATION) then
  begin
    Result := crInvalidContainer;
    GlobalFree(MemHandle);
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    EnumV := GlobalLock(MemHandle);

    {Fill return properties}
    Current := EnumV^.CurrentIndex;
    Default := EnumV^.DefaultIndex;
    ItemType := EnumV^.ItemType;

    {Prepare to list items}
    ItemSize := TWTypeSize(ItemType);
    Data := @EnumV^.ItemList[0];
    SetLength(List, EnumV^.NumItems);

    {Copy items}
    for CurItem := 0 TO EnumV^.NumItems - 1 do
    begin
      {Obtain this item}
      GetItem({%H-}Value, ItemType, Data);
      List[CurItem] := Value;
      {Move memory to the next}
      inc(Data, ItemSize);
    end;


    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end {if (Result = crSuccess)}
end;

{Returns a range capability}
function TTwainSource.GetRangeValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var Min, Max, Step, Default,
  Current: String; MemHandle: HGLOBAL): TCapabilityRet;
var
  RangeV   : pTW_RANGE;
  Container: TW_UINT16;
begin
  {Call method to get the memory to the return}
  if MemHandle = 0 then
    Result := GetCapabilityRec(Capability, MemHandle, rcGet, {%H-}Container)
  else
  begin
    Result := crSuccess;
    Container := TWON_RANGE;
  end;

  if (Result = crSuccess) and (Container <> TWON_RANGE) then
  begin
    Result := crInvalidContainer;
    GlobalFree(MemHandle);
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    RangeV := GlobalLock(MemHandle);
    {Fill return}
    ItemType := RangeV^.ItemType;
    GetItem(Min, ItemType, @RangeV^.MinValue);
    GetItem(Max, ItemType, @RangeV^.MaxValue);
    GetItem(Step, ItemType, @RangeV^.StepSize);
    GetItem(Default, ItemType, @RangeV^.DefaultValue);
    GetItem(Current, ItemType, @RangeV^.CurrentValue);

    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end {if (Result = crSuccess)}
end;

{Returns an one value capability}
function TTwainSource.GetOneValue(Capability: TW_UINT16;
  var ItemType: TW_UINT16; var Value: String;
  Mode: TRetrieveCap; MemHandle: HGLOBAL): TCapabilityRet;
var
  OneV     : pTW_ONEVALUE;
  Container: TW_UINT16;
begin
  {Call method to get the memory to the return}
  if MemHandle = 0 then
    Result := GetCapabilityRec(Capability, MemHandle, Mode, {%H-}Container)
  else
  begin
    Result := crSuccess;
    Container := TWON_ONEVALUE;
  end;

  if (Result = crSuccess) and (Container <> TWON_ONEVALUE) then
  begin
    Result := crInvalidContainer;
    GlobalFree(MemHandle);
    Exit;
  end;

  {If result was sucessfull and memory was allocated}
  if (Result = crSuccess) then
  begin
    {Obtain structure pointer}
    OneV := GlobalLock(MemHandle);
    {Fill return}
    ItemType := OneV^.ItemType;
    GetItem(Value, OneV^.ItemType, @OneV^.Item);

    {Unlock memory and unallocate}
    GlobalUnlock(MemHandle);
    GlobalFree(MemHandle);
  end {if (Result = crSuccess)}
end;

{Sets an one value capability}
function TTwainSource.SetOneValue(Capability: TW_UINT16;
  ItemType: TW_UINT16; Value: Pointer): TCapabilityRet;
var
  Data: HGLOBAL;
  OneV: pTW_ONEVALUE;
  ItemSize,ItemSize2: Integer;
begin
  {Allocate enough memory for the TW_ONEVALUE and obtain pointer}
  ItemSize := TWTypeSize(ItemType);
  //npeter: TW_ONEVALUE minimal size !!!
  //I think to meet the specifications the
  //Item's size must be at least sizeof(TW_UINT32)!
  //when I did it, some mistic errors on some drivers went gone
  if ItemSize<TWTypeSize(TWTY_UINT32) then ItemSize2:=TWTypeSize(TWTY_UINT32) else ItemSize2:=ItemSize;
  Data := GlobalAlloc(GHND, sizeof({%H-}OneV^.ItemType) + ItemSize2);
  OneV := GlobalLock(Data);

  {Fill value}
  OneV^.ItemType := ItemType;
  CopyMemory(@OneV^.Item, Value, ItemSize);
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_ONEVALUE, Data);

  {Unload memory}
  GlobalFree(Data);
end;

function TTwainSource.SetOrientation(Value: TTwainOrientation): TCapabilityRet;
const Transfer: array [TTwainOrientation] of TW_UINT16 = (TWOR_PORTRAIT, TWOR_LANDSCAPE);
var iValue: TW_UINT16;
begin
  iValue:=Transfer[value];
  Result := SetOneValue(ICAP_ORIENTATION, TWTY_UINT16, @iValue);
end;

function TTwainSource.SetPaperSize(Value: TTwainPaperSize): TCapabilityRet;
//(tpsA4, tpsA5, tpsB4, tpsB5, tpsB6, tpsUSLetter, tpsUSLegal);
const Transfer: array [TTwainPaperSize] of TW_UINT16 = (TWSS_A4LETTER, TWSS_A5, TWSS_B4, TWSS_B5LETTER, TWSS_B6, TWSS_USLETTER, TWSS_USLEGAL);
var iValue: TW_UINT16;
begin
  iValue:=Transfer[value];
  Result := SetOneValue(ICAP_SUPPORTEDSIZES, TWTY_UINT16, @iValue);
end;

{Sets a range capability}
function TTwainSource.SetRangeValue(Capability: TW_UINT16;
  ItemType: TW_UINT16; Min, Max, Step, Current: TW_UINT32): TCapabilityRet;
var
  Data: HGLOBAL;
  RangeV: pTW_RANGE;
begin
  {Allocate enough memory for the TW_RANGE and obtain pointer}
  Data := GlobalAlloc(GHND, sizeof(TW_RANGE));
  RangeV := GlobalLock(Data);

  {Fill value}
  RangeV^.ItemType := ItemType;
  RangeV^.MinValue := Min;
  RangeV^.MaxValue := Max;
  RangeV^.StepSize := Step;
  RangeV^.CurrentValue := Current;
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_RANGE, Data);

  {Unload memory}
  GlobalFree(Data);
end;

function TTwainSource.SetUndefinedImageSize(Value: Boolean): TCapabilityRet;
var iValue: TW_BOOL;
begin
  iValue := Value;
  Result := SetOneValue(ICAP_UNDEFINEDIMAGESIZE, TWTY_BOOL, @iValue);
end;

{Sets an array capability}
function TTwainSource.SetArrayValue(Capability: TW_UINT16;
  ItemType: TW_UINT16; List: TSetCapabilityList): TCapabilityRet;
var
  Data: HGLOBAL;
  EnumV: pTW_ENUMERATION;
  i, ItemSize: Integer;
  DataPt: PAnsiChar;//ccc
begin
  {Allocate enough memory for the TW_ARRAY and obtain pointer}
  ItemSize := TWTypeSize(ItemType);
  Data := GlobalAlloc(GHND, sizeof(TW_ARRAY) + ItemSize * Length(List));
  EnumV := GlobalLock(Data);

  {Fill values}
  EnumV^.ItemType := ItemType;
  EnumV^.NumItems := Length(List);

  {Copy item values}
  DataPt := @EnumV^.ItemList[0];
  for i := Low(List) TO High(List) do
  begin
    {Copy item}
    CopyMemory(DataPt, List[i], ItemSize);
    {Move to next item}
    inc(DataPt, ItemSize);
  end;
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_ARRAY, Data);

  {Unload memory}
  GlobalFree(Data);
end;

{Sets an enumeration capability}
function TTwainSource.SetEnumerationValue(Capability: TW_UINT16;
  ItemType: TW_UINT16; CurrentIndex: TW_UINT32;
  List: TSetCapabilityList): TCapabilityRet;
var
  Data: HGLOBAL;
  EnumV: pTW_ENUMERATION;
  i, ItemSize: Integer;
  DataPt: PAnsiChar;//ccc
begin
  {Allocate enough memory for the TW_ENUMERATION and obtain pointer}
  ItemSize := TWTypeSize(ItemType);
  Data := GlobalAlloc(GHND, sizeof(TW_ENUMERATION) + ItemSize * Length(List));
  EnumV := GlobalLock(Data);

  {Fill values}
  EnumV^.ItemType := ItemType;
  EnumV^.NumItems := Length(List);
  EnumV^.CurrentIndex := CurrentIndex;

  {Copy item values}
  DataPt := @EnumV^.ItemList[0];
  for i := Low(List) TO High(List) do
  begin
    {Copy item}
    CopyMemory(DataPt, List[i], ItemSize);
    {Move to next item}
    inc(DataPt, ItemSize);
  end;
  GlobalUnlock(Data);

  {Call method to set}
  Result := SetCapabilityRec(Capability, TWON_ENUMERATION, Data);

  {Unload memory}
  GlobalFree(Data);
end;

{Transfer image memory}
function TTwainSource.TransferImageMemory(var ImageHandle: HBitmap;
  PixelType: TW_INT16): TW_UINT16;
var
  {Memory buffer information from the source}
  Setup  : TW_SETUPMEMXFER;
  {Memory information from the image}
  Xfer   : TW_IMAGEMEMXFER;
  {Image processing variables}
  ImageInfo : Windows.TBitmap;
  Ptr       : PAnsiChar;//ccc
  LineLength,
  CurLine: Cardinal;
  LinePtr,
  AllocPtr  : pointer;
  DataSize,
  Readed: Cardinal;

  Index    : Cardinal;
  ItemPtr  : pRGBTriple;
  Temp     : Byte;

begin
  {Obtain information on the transference buffers}
  Result := Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_SETUPMEMXFER,
    MSG_GET, @Setup);

  {Get information on the bitmap}
  GetObject(ImageHandle, sizeof(Windows.TBitmap), @ImageInfo);
  LineLength := (((ImageInfo.bmWidth * ImageInfo.bmBitsPixel + 31) div 32) * 4);
  {Get pointer for the last line}
  CurLine := ImageInfo.bmHeight - 1;
  {%H-}DTNativeUInt(LinePtr) := DTNativeUInt(ImageInfo.bmBits) + LineLength * CurLine;
  Ptr := LinePtr;
  DataSize := 0;

  {Prepare buffer record to transfer}
  Fillchar({%H-}Xfer, SizeOf(TW_IMAGEMEMXFER), $FF);
  Xfer.Memory.Flags := TWMF_APPOWNS or TWMF_POINTER;
  Xfer.Memory.Length := Setup.Preferred;
  GetMem(AllocPtr, Setup.Preferred);
  Xfer.Memory.TheMem := AllocPtr;

  {Transfer data until done or cancelled}
  if Result = TWRC_SUCCESS then begin
    repeat
      {Retrieve another piece of memory to the pointer}
      Xfer.BytesWritten := 0;
      Result := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE,
        DAT_IMAGEMEMXFER, MSG_GET, @Xfer);
      {Test the result}
      {Piece sucessfully transfer, move to next}
      if (Result = TWRC_SUCCESS) or (Result = TWRC_XFERDONE) then
      begin
        {While we have data}
        while Xfer.BytesWritten > 0 do
        begin
          {In case the total bytes received now have more than we}
          {need to complete the line}
          if Xfer.BytesWritten + DataSize > LineLength then
          begin
            Readed := LineLength - DataSize;
            CopyMemory(Ptr, Xfer.Memory.TheMem, LineLength - DataSize);
          end
          else
          {Otherwise, continue completing the line}
          begin
            Readed := Xfer.BytesWritten;
            CopyMemory(Ptr, Xfer.Memory.TheMem, Readed);
          end;

          {Adjust}
          inc(DataSize, Readed); inc(Ptr, Readed);
          dec(Xfer.BytesWritten, Readed);
          {%H-}DTNativeUInt(Xfer.Memory.TheMem) :=
            {%H-}DTNativeUInt(Xfer.Memory.TheMem) + Readed;

          {Reached end of line}
          if (DataSize >= LineLength) then
          begin
            {Fix RGB to BGR}
            if PixelType = TWPT_RGB then
            begin
              ItemPtr := LinePtr;
              FOR Index := 1 TO ImageInfo.bmWidth DO
              begin
                Temp := ItemPtr^.rgbtRed;
                ItemPtr^.rgbtRed := ItemPtr^.rgbtBlue;
                ItemPtr^.rgbtBlue := Temp;
                inc(ItemPtr);
              end {FOR Index};
            end {if PixelType = TWPT_RGB};

            {Adjust pointers}
            {%H-}DTNativeUInt(LinePtr) := {%H-}DTNativeUInt(LinePtr) - LineLength;
            Ptr := LinePtr; dec(CurLine); DataSize := 0;

            {Call event}
            Owner.DoAcquireProgress(Self, Self.Index, ImageHandle,
              Cardinal(ImageInfo.bmHeight) - CurLine - 1,
              ImageInfo.bmHeight - 1);

          end {if DataSize >= LineLength}

        end {while Xfer.BytesWritten > 0};


        {Set again pointer to write to}
        Xfer.Memory.TheMem := AllocPtr;
      end {TWRC_SUCCESS};

    until Result <> TWRC_SUCCESS;
  end;
  {Free allocated memory}
  FreeMem(AllocPtr, Setup.Preferred);

  {Some error ocurred, free memory and returns}
  if Result <> TWRC_XFERDONE then
    DeleteObject(ImageHandle);
end;

{Prepare image memory transference}
function TTwainSource.PrepareMemXfer(var BitmapHandle: HBitmap;
  var PixelType: TW_INT16): TW_UINT16;
const
  PixelColor: Array[TTwainPixelFlavor] of Array[0..1] of Byte =
   ((0, $FF), ($FF, 00), (0, $FF));
var
  Handle: HGlobal;
  Info: TW_IMAGEINFO;
  Setup: TW_SETUPMEMXFER;
  structsize, index, Size, Blocks: Integer;
  XRes, YRes: Extended;
  Pal   : TW_PALETTE8;
  vUnit : TTwainUnit;
  vUnits: TTwainUnitSet;
  Dib   : pBitmapInfo;
  PixelFlavor: TTwainPixelFlavor;
  PixelFlavors: TTwainPixelFlavorSet;
  DC: HDC;
  Data  : Pointer;
begin
  {First of all, get information on the image being acquired}
  Result := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE, DAT_IMAGEINFO,
    MSG_GET, @Info);
  if Result <> TWRC_SUCCESS then exit;

  {Calculate image size}
  with Info do
    size := ((((ImageWidth * BitsPerPixel + 31) div 32)*4) * info.ImageLength);

  {Obtain image buffer transference sizes}
  Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_SETUPMEMXFER,
    MSG_GET, @Setup);
  blocks := (size div Integer(setup.Preferred));
  size := (blocks + 1) * Integer(setup.Preferred);

  {Prepare new bitmap}
  structsize := size + sizeof(BITMAPINFOHEADER) + 256 * sizeof(RGBQUAD);

  Handle := GlobalAlloc(GHND, StructSize);
  Dib := GlobalLock(Handle);
  Fillchar(Dib^, structsize, #0);
  {Fill image information}
  Dib^.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
  Dib^.bmiHeader.biWidth := info.ImageWidth;
  Dib^.bmiHeader.biHeight := info.ImageLength;
  {Only 1 plane supported}
  Dib^.bmiHeader.biPlanes := 1;
  Dib^.bmiHeader.biBitCount := info.BitsPerPixel;
  {No compression}
  Dib^.bmiHeader.biCompression := BI_RGB;
  Dib^.bmiHeader.biSizeImage := Size;

  {Adjust units}
  XRes := Fix32ToFloat(Info.XResolution);
  YRes := Fix32ToFloat(Info.YResolution);
  GetICapUnits({%H-}vUnit, {%H-}vUnits);
  case vUnit of
    tuInches: begin
      Dib^.bmiHeader.biXPelsPerMeter := Trunc((XRes*2.54)*100);
      Dib^.bmiHeader.biYPelsPerMeter := Trunc((YRes*2.54)*100);
      end;
    tuCentimeters: begin
      Dib^.bmiHeader.biXPelsPerMeter := Trunc(XRes*100);
      Dib^.bmiHeader.biYPelsPerMeter := Trunc(YRes*100);
      end
    else begin
      Dib^.bmiHeader.biXPelsPerMeter := 0;
      Dib^.bmiHeader.biYPelsPerMeter := 0;
    end
  end {case vUnits of};

  {Now it should setup the palette to be used by the image}
  {by either building a definied palette or retrieving the}
  {image's one}
  case (Info.PixelType) of
    TWPT_BW:
    begin
      {Only two colors are used}
      Dib^.bmiHeader.biClrUsed := 2;
      Dib^.bmiHeader.biClrImportant := 0;
      {Try obtaining the pixel flavor}
      if GetIPixelFlavor({%H-}PixelFlavor, {%H-}PixelFlavors) <> crSuccess then
        PixelFlavor := tpfChocolate;
      {Set palette colors}
      for Index := 0 to 1 do
      begin
        Dib^.bmiColors[Index].rgbRed := PixelColor[PixelFlavor][Index];
        Dib^.bmiColors[Index].rgbGreen := PixelColor[PixelFlavor][Index];
        Dib^.bmiColors[Index].rgbBlue := PixelColor[PixelFlavor][Index];
        Dib^.bmiColors[Index].rgbReserved := 0;
      end;

    end;
    TWPT_GRAY:
    begin
      {Creates a 256 shades of gray palette}
      Dib^.bmiHeader.biClrUsed := 256;
      for index := 0 to 255 do
      begin
        Dib^.bmiColors[index].rgbRed := index;
        Dib^.bmiColors[index].rgbGreen := index;
        Dib^.bmiColors[index].rgbBlue := index;
        Dib^.bmiColors[index].rgbReserved := 0;
      end {for i}
    end;
    TWPT_RGB: Dib^.bmiHeader.biClrUsed := 0;
    else
    begin
      {Try obtaining the palette}
      if Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_PALETTE8,
        MSG_GET, @Pal) <> TWRC_SUCCESS then
      begin
        {If the source did not provide a palette, uses shades of gray here}
        Dib^.bmiHeader.biClrUsed := 256;
        for index := 0 to 255 do
        begin
          Dib^.bmiColors[index].rgbRed := index;
          Dib^.bmiColors[index].rgbGreen := index;
          Dib^.bmiColors[index].rgbBlue := index;
          Dib^.bmiColors[index].rgbReserved := 0;
        end {for i}
      end
      else
      begin
        {Uses source palette here}
        Dib^.bmiHeader.biClrUsed := Pal.NumColors;
        for Index := 0 TO Pal.NumColors - 1 do
        begin
          Dib^.bmiColors[index].rgbRed := pal.Colors[index].Channel1;
          Dib^.bmiColors[index].rgbGreen := pal.Colors[index].Channel2;
          Dib^.bmiColors[index].rgbBlue := pal.Colors[index].Channel3;
          Dib^.bmiColors[index].rgbReserved := 0;
        end {for Index}
      end {if Owner.TwainProc(AppInfo...}

    end {case else};
  end {case Info.PixelType};

  {Creates the bitmap}
  DC := GetDC(Owner.VirtualWindow);
  {%H-}DTNativeUInt({%H-}Data) := DTNativeUInt(Dib) + Dib^.bmiHeader.biSize +
    (Dib^.bmiHeader.biClrUsed * sizeof(RGBQUAD));
  BitmapHandle := CreateDIBSection(DC, Dib^, DIB_RGB_COLORS, Data, 0, 0);
  ReleaseDC(Owner.VirtualWindow, DC);
  PixelType := Info.PixelType;

  {Unlock and free data}
  GlobalUnlock(Handle);
  GlobalFree(Handle);
end;

{Method to transfer the images}
procedure TTwainSource.TransferImages();
var
  {To test if the image transfer is done}
  Cancel, Done   : Boolean;
  {Return code from Twain method}
  rc     : TW_UINT16;
  {Handle to the native Device independent Image (DIB)}
  hNative: TW_UINT32;
  {Pending transfers structure}
  PendingXfers: TW_PENDINGXFERS;
  {File transfer info}
  Info: TW_SETUPFILEXFER;
  {Image handle and pointer}
  ImageHandle: HBitmap;
  PixelType  : TW_INT16;
begin
  {Set the transfer mode}
  //npeter:
  //on a HP driver I got error events
  //when it was set above state 5;
  //commented out
  // ChangeTransferMode(TransferMode);

  Cancel := FALSE; {Testing if it was cancelled}
  Done := FALSE;  {Initialize done variable}

  {Obtain all the images from the source}
  repeat
    {Transfer depending on the transfer mode}
    case TransferMode of
      {Native transfer, the source creates the image thru a device}
      {dependent image}
      ttmNative:
      begin
        {Call method to obtain the image}
        hNative := 0;
        rc := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE,
          DAT_IMAGENATIVEXFER, MSG_GET, @hNative);
      end {case ttmNative};
      {File transfering, the source should create a file with}
      {the acquired image}
      ttmFile:
      begin
        {Event to allow user to set the file transfer information}
        if Assigned(Owner.OnSourceSetupFileXfer) then
          Owner.OnSourceSetupFileXfer(Owner, Index);
        Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_SETUPFILEXFER,
          MSG_GET, @Info);
        {Call method to make source acquire and create file}
        rc := Owner.TwainProc(AppInfo, @Structure, DG_IMAGE,
          DAT_IMAGEFILEXFER, MSG_GET, nil);
      end {case ttmFile};
      {Memory buffer transfers}
      ttmMemory:
      begin
        {Prepare for memory transference}
        rc := PrepareMemXfer({%H-}ImageHandle, {%H-}PixelType);
        {If the image was sucessfully prepared to be transfered, it's}
        {now time to transfer it}
        if rc = TWRC_SUCCESS then rc := TransferImageMemory(ImageHandle,
          PixelType);
      end
      {Unknown transfer mode ?}
      else Rc := 0;
    end;

    {Twain call to transfer image return}
    case rc of
      {Transfer sucessfully done}
      TWRC_XFERDONE:
        case TransferMode of
          {Native transfer sucessfull}
          ttmNative: ReadNative(hNative, Cancel);
          {File transfer sucessfull}
          ttmFile: ReadFile(Info.FileName, Info.Format, Cancel);
          {Memory transfer sucessfull}
          ttmMemory: ReadMemory(ImageHandle, Cancel);
        end {case TransferMode, TWRC_XFERDONE};
      {User cancelled the transfers}
      TWRC_CANCEL:
      begin
        {Acknowledge end of transfer}
        Done := TRUE;
        Cancel := TRUE;
        {Call event, if avaliable}
        if Assigned(Owner.OnAcquireCancel) then
          Owner.OnAcquireCancel(Owner, Index)
      end
      else {Unknown return or error}
        if Assigned(Owner.OnAcquireError) then
          Owner.OnAcquireError(Owner, Index, Rc, GetReturnStatus())
    end;

    {Check if there are pending transfers}
    if not Done then
      Done := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
        DAT_PENDINGXFERS, MSG_ENDXFER, @PendingXfers) <> TWRC_SUCCESS) or
        (PendingXfers.Count = 0);

    {If user has cancelled}
    if not Done and Cancel then
      Done := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
        DAT_PENDINGXFERS, MSG_RESET, @PendingXfers) = TWRC_SUCCESS);

  until Done;

  {Disable source}
  Enabled := False;

  {All documents have been transfered}
  if Assigned(Owner.OnTransferComplete) then
    Owner.OnTransferComplete(Owner, Index, Cancel);

  Owner.RefreshVirtualWindow;
end;

{Returns the number of colors in the DIB}
function DibNumColors (pv: Pointer): Word;
var
  Bits: Integer;
  lpbi: PBITMAPINFOHEADER absolute pv;
  lpbc: PBITMAPCOREHEADER absolute pv;
begin
  //With the BITMAPINFO format headers, the size of the palette
  //is in biClrUsed, whereas in the BITMAPCORE - style headers, it
  //is dependent on the bits per pixel ( = 2 raised to the power of
  //bits/pixel).
  if (lpbi^.biSize <> sizeof(BITMAPCOREHEADER)) then
  begin
    if (lpbi^.biClrUsed <> 0) then
    begin
      result := lpbi^.biClrUsed;
      exit;
    end;
    Bits := lpbi^.biBitCount;
  end
  else
     Bits := lpbc^.bcBitCount;

  {Test bits to return}
  case (Bits) of
    1: Result := 2;
    4: Result := 16;
    8: Result := 256;
    else Result := 0;
  end {case};

end;

{Converts from TWain TW_UINT16 to TTwainFormat}
function TwainToTTwainFormat(Value: TW_UINT16): TTwainFormat;
begin
  Case Value of
    TWFF_TIFF     : Result := tfTIFF;
    TWFF_PICT     : Result := tfPict;
    TWFF_BMP      : Result := tfBMP;
    TWFF_XBM      : Result := tfXBM;
    TWFF_JFIF     : Result := tfJPEG;
    TWFF_FPX      : Result := tfFPX;
    TWFF_TIFFMULTI: Result := tfTIFFMulti;
    TWFF_PNG      : Result := tfPNG;
    TWFF_SPIFF    : Result := tfSPIFF;
    TWFF_EXIF     : Result := tfEXIF;
    else            Result := tfUnknown;
  end {case Value of}
end;

{Reads the file image}
procedure TTwainSource.ReadFile(Name: TW_STR255; Format: TW_UINT16;
  var Cancel: Boolean);
begin
  {Call event, if set}
  if Assigned(Owner.OnSourceFileTransfer) then
    Owner.OnSourceFileTransfer(Self, Index, Name, TwainToTTwainFormat(Format),
      Cancel);
end;

{Call event for memory image}
procedure TTwainSource.ReadMemory(Image: HBitmap; var Cancel: Boolean);
begin
  Owner.DoTwainAcquire(Owner, Index, Image, Cancel);
end;

{Reads a native image}
procedure TTwainSource.ReadNative(Handle: TW_UINT32; var Cancel: Boolean);
var
  DibInfo: ^TBITMAPINFO;
  ColorTableSize: Integer;
  lpBits: PAnsiChar;//ccc
  DC: HDC;
  BitmapHandle: HBitmap;
begin

  {Get image information pointer and size}
  DibInfo := GlobalLock(Handle);
  ColorTableSize := (DibNumColors(DibInfo) * SizeOf(RGBQUAD));

  {Get data memory position}
  lpBits := PAnsiChar(DibInfo);//ccc
  //{$IFDEF FPC}
  Inc(lpBits, DibInfo.bmiHeader.biSize);
  //{$ELSE}ccc
  //DELPHI BUG - due to wrong PChar definition
  //Inc(lpBits, DibInfo.bmiHeader.biSize div 2);
  //{$ENDIF}
  Inc(lpBits, ColorTableSize);
  //lpBits := PAnsiChar(DibInfo^.bmiColors);//ccc

  {Creates the bitmap}
  DC := GetDC(Owner.VirtualWindow);

  BitmapHandle := CreateDIBitmap(DC, DibInfo.bmiHeader, CBM_INIT,
     lpBits, DibInfo^, DIB_RGB_COLORS);
  ReleaseDC(Owner.VirtualWindow, DC);

  Owner.DoTwainAcquire(Owner, Index, BitmapHandle, Cancel);

  {Free bitmap}
  GlobalUnlock(Handle);
  GlobalFree(Handle);
end;

{Setup file transfer}
function TTwainSource.SetupFileTransfer(Filename: String;
  Format: TTwainFormat): Boolean;
const
  FormatToTwain: Array[TTwainFormat] of TW_UINT16 = (TWFF_TIFF,
    TWFF_PICT, TWFF_BMP, TWFF_XBM, TWFF_JFIF, TWFF_FPX, TWFF_TIFFMULTI,
    TWFF_PNG, TWFF_SPIFF, TWFF_EXIF, 0);
var
  FileTransferInfo: TW_SETUPFILEXFER;
begin
  {Source must be loaded to set things}
  if (Loaded) then
  begin
    {Prepare structure}
    FileTransferInfo.FileName := StrToStr255({$IFDEF UNICODE}RawByteString{$ENDIF}(FileName));
    FileTransferInfo.Format := FormatToTwain[Format];

    {Call method}
    Result := (Owner.TwainProc(AppInfo, @Structure, DG_CONTROL,
      DAT_SETUPFILEXFER, MSG_SET, @FileTransferInfo) = TWRC_SUCCESS);
  end
  else Result := FALSE;  {Could not set file transfer with source unloaded}
end;

{Set the number of images that the application wants to receive}
function TTwainSource.SetCapXferCount(Value: SmallInt): TCapabilityRet;
begin
  {Call method to set the value}
  Result := SetOneValue(CAP_XFERCOUNT, TWTY_UINT16, @Value);
end;

{Returns the number of images that the source will return}
function TTwainSource.GetCapXferCount(var Return: SmallInt;
  Mode: TRetrieveCap): TCapabilityRet;
var
  {Will hold the capability information}
  ItemType: TW_UINT16;
  Value   : String;
begin
  {Call method to return information}
  Result := GetOneValue(CAP_XFERCOUNT, {%H-}ItemType, {%H-}Value, Mode);
  {Item type must be of TW_UINT16}
  if (Result = crSuccess) and (ItemType <> TWTY_INT16) then
    Result := crUnsupported;
  {If everything gone ok, fill result}
  if Result = crSuccess then Return := StrToIntDef(Value, -1);
end;

{Set the unit measure}
function TTwainSource.SetICapUnits(Value: TTwainUnit): TCapabilityRet;
//npeter
//the TTwainUnit is byte!!!
//so we have to convert it to TW_UINT16
//before this fix I was not able to set this capability
//on a HP driver
const Transfer: Array[TTwainUnit] of TW_UINT16 =
       (TWUN_INCHES, TWUN_CENTIMETERS, TWUN_PICAS, TWUN_POINTS, TWUN_TWIPS, TWUN_PIXELS, TWUN_INCHES);
var
  iValue: TW_UINT16;
begin
  ivalue:=Transfer[Value];
  Result := SetOneValue(ICAP_UNITS, TWTY_UINT16, @iValue);
end;

{Convert from Twain to TTwainPixelFlavor}
function TwainToTTwainPixelFlavor(Value: TW_UINT16): TTwainPixelFlavor;
begin
  {Test the value to make the convertion}
  case Value of
    TWPF_CHOCOLATE: Result := tpfChocolate;
    TWPF_VANILLA  : Result := tpfVanilla;
  else Result := tpfUnknown;
  end {case Value}
end;

{Convert from Twain to TTwainUnit}
function TwainToTTwainUnit(Value: TW_UINT16): TTwainUnit;
begin
  {Test the value to make the convertion}
  case Value of
    TWUN_INCHES     : Result := tuInches;
    TWUN_CENTIMETERS: Result := tuCentimeters;
    TWUN_PICAS      : Result := tuPicas;
    TWUN_POINTS     : Result := tuPoints;
    TWUN_TWIPS      : Result := tuTwips;
    TWUN_PIXELS     : Result := tuPixels;
  else Result := tuUnknown;
  end {case Value}
end;

{Retrieve the unit measure for all quantities}
function TTwainSource.GetICapUnits(var Return: TTwainUnit;
  var Supported: TTwainUnitSet; Mode: TRetrieveCap): TCapabilityRet;
var
  ItemType: TW_UINT16;
  List    : TGetCapabilityList;
  Current, i,
  Default : Integer;
begin
  {Call method to get result}
  Result := GetEnumerationValue(ICAP_UNITS, {%H-}ItemType, {%H-}List, {%H-}Current, {%H-}Default,
    Mode);
  if ItemType <> TWTY_UINT16 then Result := crUnsupported;

  {If it was sucessfull, return values}
  if Result = crSuccess then
  begin
    {Make list}
    for i := Low(List) to High(List) do
      Include(Supported, TwainToTTwainUnit(StrToIntDef(List[i], -1)));
    {Return values depending on the mode}
    if Mode = rcGetDefault then
      Return := TwainToTTwainUnit(StrToIntDef(List[Default], -1))
    else
      Return := TwainToTTwainUnit(StrToIntDef(List[Current], -1));
  end {if Result = crSuccess}

end;

{Retrieve the pixel flavor values}
function TTwainSource.GetIPixelFlavor(var Return: TTwainPixelFlavor;
  var Supported: TTwainPixelFlavorSet; Mode: TRetrieveCap): TCapabilityRet;
var
  ItemType: TW_UINT16;
  List    : TGetCapabilityList;
  Current, i,
  Default : Integer;
begin
  {Call method to get result}
  Result := GetEnumerationValue(ICAP_PIXELFLAVOR, {%H-}ItemType, {%H-}List, {%H-}Current,
    {%H-}Default, Mode);
  if ItemType <> TWTY_UINT16 then Result := crUnsupported;

  {If it was sucessfull, return values}
  if Result = crSuccess then
  begin
    {Make list}
    for i := Low(List) to High(List) do
      Include(Supported, TwainToTTwainPixelFlavor(StrToIntDef(List[i], -1)));
    {Return values depending on the mode}
    if Mode = rcGetDefault then
      Return := TwainToTTwainPixelFlavor(StrToIntDef(List[Default], -1))
    else
      Return := TwainToTTwainPixelFlavor(StrToIntDef(List[Current], -1));
  end {if Result = crSuccess}
end;

function TTwainSource.SetIPixelFlavor(Value: TTwainPixelFlavor): TCapabilityRet;
//npeter
//the TTwainPixelFlavor is byte!!!
//so we have to convert it to TW_UINT16
//before this fix I was not able to set this capability
//on a HP driver
const Transfer: array [TTwainPixelFlavor] of TW_UINT16 = (TWPF_CHOCOLATE,TWPF_VANILLA,TWPF_CHOCOLATE);
var iValue: TW_UINT16;
begin
  iValue:=Transfer[value];
  Result := SetOneValue(ICAP_PIXELFLAVOR, TWTY_UINT16, @iValue);
end;

{Convert from Twain to TTwainPixelType}
function TwainToTTwainPixelType(Value: TW_UINT16): TTwainPixelType;
begin
  {Test the value to make the convertion}
  case Value of
    TWPT_BW         : Result := tbdBw;
    TWPT_GRAY       : Result := tbdGray;
    TWPT_RGB        : Result := tbdRgb;
    TWPT_BGR        : Result := tbdBgr;
    TWPT_PALETTE    : Result := tbdPalette;
    TWPT_CMY        : Result := tbdCmy;
    TWPT_CMYK       : Result := tbdCmyk;
    TWPT_YUV        : Result := tbdYuv;
    TWPT_YUVK       : Result := tbdYuvk;
    TWPT_CIEXYZ     : Result := tbdCieXYZ;
  else Result := tbdUnknown;
  end {case Value}
end;

{Returns pixel type values}
function TTwainSource.GetIPixelType(var Return: TTwainPixelType;
  var Supported: TTwainPixelTypeSet; Mode: TRetrieveCap): TCapabilityRet;
var
  ItemType: TW_UINT16;
  List    : TGetCapabilityList;
  Current, i,
  Default : Integer;
begin
  {Call method to get result}
  Result := GetEnumerationValue(ICAP_PIXELTYPE, {%H-}ItemType, {%H-}List, {%H-}Current,
    {%H-}Default, Mode);
  if ItemType <> TWTY_UINT16 then Result := crUnsupported;

  {If it was sucessfull, return values}
  if Result = crSuccess then
  begin
    {Make list}
    for i := Low(List) to High(List) do
      Include(Supported, TwainToTTwainPixelType(StrToIntDef(List[i], -1)));
    {Return values depending on the mode}
    if Mode = rcGetDefault then
      Return := TwainToTTwainPixelType(StrToIntDef(List[Default], -1))
    else
      Return := TwainToTTwainPixelType(StrToIntDef(List[Current], -1));
  end {if Result = crSuccess}
end;

{Set the pixel type value}
function TTwainSource.SetIPixelType(Value: TTwainPixelType): TCapabilityRet;
//npeter
//the TTwainPixelType is byte!!!
//so we have to convert it to TW_UINT16
//before this fix occasionally I was not able to set this capability
//on a HP driver
var ivalue: smallint;
begin
 ivalue:=ord(value);
 Result := SetOneValue(ICAP_PIXELTYPE, TWTY_UINT16, @iValue);
end;

{Returns bitdepth values}
function TTwainSource.GetIBitDepth(var Return: Word;
  var Supported: TTwainBitDepth; Mode: TRetrieveCap): TCapabilityRet;
var
  ItemType: TW_UINT16;
  List    : TGetCapabilityList;
  Current, i,
  Default : Integer;
begin
  {Call GetOneValue to obtain this property}
  Result := GetEnumerationValue(ICAP_BITDEPTH, {%H-}ItemType, {%H-}List, {%H-}Current,
    {%H-}Default, Mode);
  if ItemType <> TWTY_UINT16 then Result := crUnsupported;

  {In case everything went ok, fill parameters}
  if Result = crSuccess then
  begin
    {Build bit depth list}
    SetLength(Supported, Length(List));
    FOR i := LOW(List) TO HIGH(List) DO
      Supported[i] := StrToIntDef(List[i], -1);
    {Return values depending on the mode}
    if Mode = rcGetDefault then Return := StrToIntDef(List[Default], -1)
    else Return := StrToIntDef(List[Current], -1);
  end {if Result = crSuccess}
end;

{Set current bitdepth value}
function TTwainSource.SetIBitDepth(Value: Word): TCapabilityRet;
begin
  Result := SetOneValue(ICAP_BITDEPTH, TWTY_UINT16, @Value);
end;

{Returns physical width}
function TTwainSource.GetIPhysicalWidth(var Return: Extended;
  Mode: TRetrieveCap): TCapabilityRet;
var
  Handle: HGlobal;
  OneV  : pTW_ONEVALUE;
  Container: TW_UINT16;
begin
  {Obtain handle to data from this capability}
  Result := GetCapabilityRec(ICAP_PHYSICALWIDTH, {%H-}Handle, {%H-}Mode, {%H-}Container);
  if Result = crSuccess then
  begin
    {Obtain data}
    OneV := GlobalLock(Handle);
    if OneV^.ItemType <> TWTY_FIX32 then Result := crUnsupported
    else Return := Fix32ToFloat(pTW_FIX32(@OneV^.Item)^);
    {Free data}
    GlobalUnlock(Handle);
    GlobalFree(Handle);
  end;
end;

{Returns physical height}
function TTwainSource.GetIPhysicalHeight(var Return: Extended;
  Mode: TRetrieveCap): TCapabilityRet;
var
  Handle: HGlobal;
  OneV  : pTW_ONEVALUE;
  Container: TW_UINT16;
begin
  {Obtain handle to data from this capability}
  Result := GetCapabilityRec(ICAP_PHYSICALHEIGHT, {%H-}Handle, {%H-}Mode, {%H-}Container);
  if Result = crSuccess then
  begin
    {Obtain data}
    OneV := GlobalLock(Handle);
    if OneV^.ItemType <> TWTY_FIX32 then Result := crUnsupported
    else Return := Fix32ToFloat(pTW_FIX32(@OneV^.Item)^);
    {Free data}
    GlobalUnlock(Handle);
    GlobalFree(Handle);
  end;
end;

{Returns a resolution}
function TTwainSource.GetResolution(Capability: TW_UINT16; var Return: Extended;
  var Values: TTwainResolution; Mode: TRetrieveCap): TCapabilityRet;
var
  Handle: HGlobal;
  EnumV:  pTW_ENUMERATION;
  Container: TW_UINT16;
  Item: pTW_FIX32;
  i   : Integer;
begin
  {Obtain handle to data from this capability}
  Result := GetCapabilityRec(Capability, {%H-}Handle, Mode, {%H-}Container);
  if Result = crSuccess then
  begin
    {Obtain data}
    //npeter
    //the "if" is just for sure!
    if (Container<>TWON_ENUMERATION) and (Container<>TWON_ARRAY) then
     begin
      result:=crUnsupported;
      exit;
     end;

    EnumV := GlobalLock(Handle);
    if EnumV^.ItemType <> TWTY_FIX32 then Result := crUnsupported
    else begin
      {Set array size and pointer to the first item}
      Item := @EnumV^.ItemList[0];
      SetLength(Values, EnumV^.NumItems);
      {Fill array}
      FOR i := 1 TO EnumV^.NumItems DO
      begin
        {Fill array with the item}
        Values[i - 1] := Fix32ToFloat(Item^);
        {Move to next item}
        inc(Item);
      end {FOR i};

      {Fill return}

      //npeter
      //DefaultIndex and CurrentIndex valid for enum only!
      //I got nice AV with an old Mustek scanner which uses TWON_ARRAY
      //i return 0 in this case (may be not the best solution, but not AV at least :-)
      if (Container<>TWON_ARRAY) then
       begin
        if Mode = rcGetDefault then Return := Values[EnumV^.DefaultIndex]
        else Return := Values[EnumV^.CurrentIndex];
       end
      else return:=0;
    end;
    {Free data}
    GlobalUnlock(Handle);
    GlobalFree(Handle);
  end;
end;

{Sets X resolution}
function TTwainSource.SetIXResolution(Value: Extended): TCapabilityRet;
var
  Fix32: TW_FIX32;
begin
  Fix32 := FloatToFix32(Value);
  Result := SetOneValue(ICAP_XRESOLUTION, TWTY_FIX32, @Fix32);
end;

{Sets Y resolution}
function TTwainSource.SetIYResolution(Value: Extended): TCapabilityRet;
var
  Fix32: TW_FIX32;
begin
  Fix32 := FloatToFix32(Value);
  Result := SetOneValue(ICAP_YRESOLUTION, TWTY_FIX32, @Fix32);
end;

{Returns X resolution}
function TTwainSource.GetIXResolution(var Return: Extended;
  var Values: TTwainResolution; Mode: TRetrieveCap): TCapabilityRet;
begin
  Result := GetResolution(ICAP_XRESOLUTION, Return, Values, Mode);
end;

{Returns Y resolution}
function TTwainSource.GetIYResolution(var Return: Extended;
  var Values: TTwainResolution; Mode: TRetrieveCap): TCapabilityRet;
begin
  Result := GetResolution(ICAP_YRESOLUTION, Return, Values, Mode);
end;

{Returns if user interface is controllable}
function TTwainSource.GetUIControllable(var Return: Boolean): TCapabilityRet;
var
  ItemType: TW_UINT16;
  Value   : String;
begin
  {Try to obtain value and make sure it is of type TW_BOOL}
  Result := GetOneValue(CAP_UICONTROLLABLE, {%H-}ItemType, {%H-}Value, rcGet);
  if (Result = crSuccess) and (ItemType <> TWTY_BOOL) then
    Result := crUnsupported;
  {Return value, by checked the return value from GetOneValue}
  if Result = crSuccess then Return := (Value = '1');
end;

{Returns if feeder is loaded}
function TTwainSource.GetFeederLoaded(var Return: Boolean): TCapabilityRet;
var
  ItemType: TW_UINT16;
  Value   : String;
begin
  {Try to obtain value and make sure it is of type TW_BOOL}
  Result := GetOneValue(CAP_FEEDERLOADED, {%H-}ItemType, {%H-}Value, rcGet);
  if (Result = crSuccess) and (ItemType <> TWTY_BOOL) then
    Result := crUnsupported;
  {Return value, by checked the return value from GetOneValue}
  if Result = crSuccess then Return := (Value = '1');
end;

{Returns if feeder is enabled}
function TTwainSource.GetFeederEnabled(var Return: Boolean): TCapabilityRet;
var
  ItemType: TW_UINT16;
  Value   : String;
begin
  {Try to obtain value and make sure it is of type TW_BOOL}
  Result := GetOneValue(CAP_FEEDERENABLED, {%H-}ItemType, {%H-}Value, rcGet);
  if (Result = crSuccess) and (ItemType <> TWTY_BOOL) then
    Result := crUnsupported;
  {Return value, by checked the return value from GetOneValue}
  if Result = crSuccess then Return := (Value = '1');
end;

{Set if feeder is enabled}
function TTwainSource.SetFeederEnabled(Value: WordBool): TCapabilityRet;
begin
  {Call SetOneValue to set value}
  Result := SetOneValue(CAP_FEEDERENABLED, TWTY_BOOL, @Value);
end;


{Returns if autofeed is enabled}
function TTwainSource.GetAutofeed(var Return: Boolean): TCapabilityRet;
var
  ItemType: TW_UINT16;
  Value   : String;
begin
  {Try to obtain value and make sure it is of type TW_BOOL}
  Result := GetOneValue(CAP_AUTOFEED, {%H-}ItemType, {%H-}Value, rcGet);
  if (Result = crSuccess) and (ItemType <> TWTY_BOOL) then
    Result := crUnsupported;
  {Return value, by checked the return value from GetOneValue}
  if Result = crSuccess then Return := (Value = '1');
end;

function TTwainSource.SetAutoBorderDetection(Value: Boolean): TCapabilityRet;
var iValue: TW_BOOL;
begin
  iValue := Value;
  Result := SetOneValue(ICAP_AUTOMATICBORDERDETECTION, TWTY_BOOL, @iValue);
end;

{Set if autofeed is enabled}
function TTwainSource.SetAutoFeed(Value: WordBool): TCapabilityRet;
begin
  {Call SetOneValue to set value}
  Result := SetOneValue(CAP_AUTOFEED, TWTY_BOOL, @Value);
end;


function TTwainSource.SetAutoRotate(Value: Boolean): TCapabilityRet;
var iValue: TW_BOOL;
begin
  iValue := Value;
  Result := SetOneValue(ICAP_AUTOMATICROTATE, TWTY_BOOL, @iValue);
end;

function TTwainSource.SetAutoDeskew(Value: Boolean): TCapabilityRet;
var iValue: TW_BOOL;
begin
  iValue := Value;
  Result := SetOneValue(ICAP_AUTOMATICDESKEW, TWTY_BOOL, @iValue);
end;

function TTwainSource.SetAutoSize(Value: TTwainAutoSize): TCapabilityRet;
const Transfer: array [TTwainAutoSize] of TW_UINT16 = (TWAS_NONE, TWAS_AUTO, TWAS_CURRENT);
var iValue: TW_UINT16;
begin
  iValue:=Transfer[value];
  Result := SetOneValue(ICAP_AUTOSIZE, TWTY_UINT16, @iValue);
end;

{Used with property PendingXfers}
function TTwainSource.GetPendingXfers: TW_INT16;
var
  PendingXfers: TW_PENDINGXFERS;
begin
  if Loaded and Enabled then
  begin
    {Call method to retrieve}
    if Owner.TwainProc(AppInfo, @Structure, DG_CONTROL, DAT_PENDINGXFERS,
      MSG_GET, @PendingXfers) = TWRC_SUCCESS then
      Result := PendingXfers.Count
    else Result := ERROR_INT16; {Some error ocurred while calling message}
  end
  else Result := ERROR_INT16;  {Source not loaded/enabled}
end;

{Virtual window procedure handler}
function VirtualWinProc(Handle: THandle; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LResult; stdcall;

  {Returns the TCustomDelphiTwain object}
  function Obj: TCustomDelphiTwain;
  begin
    DTNativeUInt(Result) := GetWindowLong(Handle, GWL_USERDATA);
  end {function};

var
  Twain: TCustomDelphiTwain;
  i    : Integer;
  Msg  : TMsg;
begin
  {Tests for the message}
  case uMsg of
    {Creation of the window}
    WM_CREATE:
      {Stores the TCustomDelphiTwain object handle}
      with {%H-}pCreateStruct(lParam)^ do
        SetWindowLong(Handle, GWL_USERDATA, {%H-}Longint(lpCreateParams));
    {case} else
    begin
      {Try to obtain the current object pointer}
      Twain := Obj;

      if Assigned(Twain) then
        {If there are sources loaded, we need to verify}
        {this message}
       if (Twain.SourcesLoaded > 0) then
        begin
          {Convert parameters to a TMsg}
          Msg := MakeMsg(Handle, uMsg, wParam, lParam);
          {Tell about this message}
          FOR i := 0 TO Twain.SourceCount - 1 DO
            if ((Twain.Source[i].Loaded) and (Twain.Source[i].Enabled)) then
              if Twain.Source[i].ProcessMessage(Msg) then
              begin
                {Case this was a message from the source, there is}
                {no need for the default procedure to process}
                Result := 0;
                Exit;
              end;

        end {if (Twain.SourcesLoaded > 0)}


    end {case Else}
  end {case uMsg of};

  {Calls method to handle}
  Result := DefWindowProc(Handle, uMsg, wParam, lParam);
end;


//npeter: 2004.01.12
//sets the acquired area
function TTwainSource.SetImagelayoutFrame(const fLeft, fTop, fRight,
  fBottom: double): TCapabilityRet;
var ImageLayout: TW_IMAGELAYOUT;
begin
 if not Loaded then
  begin
   Result := crInvalidState;  {In case the source is not loaded}
   exit;
  end;

 fillchar({%H-}ImageLayout,sizeof(TW_IMAGELAYOUT),0);
 with ImageLayout.Frame do
  begin
   Left:=FloatToFIX32(fLeft);
   Top:=FloatToFIX32(fTop);
   Right:=FloatToFIX32(fRight);
   Bottom:=FloatToFIX32(fBottom);
  end;
 {Call method and store return}
 Result := ResultToCapabilityRec(Owner.TwainProc(AppInfo, @Structure,
      DG_IMAGE, DAT_IMAGELAYOUT, MSG_SET, @ImageLayout));
end;

//npeter: 2004.01.12
//enable/disable progress indicators
function TTwainSource.SetIndicators(Value: boolean): TCapabilityRet;
begin
  {Call SetOneValue to set value}
  Result := SetOneValue(CAP_INDICATORS, TWTY_BOOL, @Value);
end;

end.

