# delphi-twain
Twain features for Delphi

# Installation

DelphiTwain is a runtime library. There are no visual components that have to be installed. Just add the source code directory to your library path and start using DelphiTwain.

The library is initially created by © Gustavo Daud and modified by Nemeth Peter and vcldeveloper.

# Library design

- Full html help for the component classes. The library is able to fully access Twain capabilities.
- VCL, LCL and FireMonkey support (Windows-only).
- TDelphiTwain 1.4 is not a TComponent descendand any more - you can't install it into the Delphi pallete and use it as a non-visual component any more. You have to use it from code only and free it by yourself (see the DEMO).
  For VCL add DelphiTwain and DelphiTwain_VCL to the uses clause.
  For FireMonkey add DelphiTwain and DelphiTwain_FMX units to the uses clause.
- Acquiring images is easy as a few line codes.
- Direct access to various twain features.
- Showcases making it easy to learn.
- Supported Delphi versions:
  VCL: Delphi 6 and newer
  FireMonkey: Delphi XE2 and newer.
- Supported Lazarus versions: 1.0.0 and newer.

# Kluug.net modifications

- Version 1.4: FireMonkey support (XE2+) - thanks to Frediano Palazzi for financially supporting this modification.
TDelphiTwain is not a TComponent descendand any more - you have to use it from code only and free it by yourself.
For FireMonkey add DelphiTwain and DelphiTwain_FMX units to the uses clause. For VCL add DelphiTwain and DelphiTwain_VCL to the uses clause.
- Version 1.3: New method OnTransferComplete: fired when all documents are scanned or the scan is canceled. Thanks to Andrei Galatyn.
Fixed function FloatToFix32 - thanks to Chad Berchek.
- Version 1.2: bug fixies: Color problems solved (thanks to Marco & Christian). All modes are supported again.
- Version 1.1: bug fixies: TWAIN drivers did not respond - now both WIA and TWAIN can be used. Only native mode is supported!
- Version 1.0: Lazarus support
- Version 1.0: experimental 64bit compiler support (you need 64bit scanner and TWAIN drivers: http://sourceforge.net/projects/twain-dsm/ and tweak the line 778 in DelphiTwain.pas:
 ```
 fHandle := Windows.LoadLibrary(PChar(TwainDirectory + TWAINLIBRARY));
 ```
 to the new DLL). 64bit code has not been thoroughly tested.
 If your 64bit application does not show any TWAIN sources, you are probably missing a 64bit TWAIN driver for your scanner.

# Original versions

Original code: delphitwain.sourceforge.net
Unicode modifications: stackoverflow.com/questions/2059343/twain-scanning-components-for-delphi
Additional modifications: kluug.net/delphitwain.php
