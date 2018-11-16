library dsmodel109;

  {-MBP-module voor bestrijdingsmiddelen.
    Reus, J.A.W.A., 1992: Milieumeetlat voor bestrijdingsmiddelen - Toetsing en
    bijstelling, Centrum voor Landbouw en Milieu 1992-96, Utrecht.
    KVI nov. 2001 }

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{.$define test}

uses
  ShareMem,
  {$ifdef test} forms, {$endif} windows, SysUtils, Classes, LargeArrays,
  ExtParU, USpeedProc, uDCfunc,UdsModel, UdsModelS, xyTable, DUtils, uError;

Const
  cModelID    = 109;  {-Key to this model (=unique-ID)}

  {-Mapping of dependent variable vector (=aantal te integreren snelheden)}
  cNrOfDepVar = 1;    {-Length of dependent variable vector}

  cMBP        = 1;    {-Hoeveelheid milieu-belasting-punten (MBP)}
  
  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;
  
  {***** Used when booted for shell-usage ***}
  cnRP    = 4;  {-Nr. of RP-time-series that must be supplied by the shell in
                  EP[ indx-1 ].}
  cnSQ    = 0;  {-Nr. of point-time-series that must be supplied by the shell
                  in EP[ indx-1 ]. REM: point- sources niet aan de orde bij
                  stikstof-uitspoeling!}
  cnRQ    = 0;  {-Nr. of line-time-series that must be supplied
                  by the shell in EP[ indx-1 ]}

  {-Mapping of EP[cEP0]}
  cNrXIndepTblsInEP0 = 9;  {-Nr. of XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;  {-Nr. of Xdep-tables   in EP[cEP0]}

  {-EP[cEP0]: xIndep-Table numbering; 0&1 are reserved}
  cTb_MinMaxValKeys   = 2; {-Min/Max. waarden van shell-gegevens}
  cTb_BodemPrmtrsA    = 3; {-Bodem-gerelateerde parameters: akkerbouw}
  cTb_BodemPrmtrsB    = 4; {-Bodem-gerelateerde parameters: Bos}
  cTb_BodemPrmtrsG    = 5; {-Bodem-gerelateerde parameters: Gras}
  cTb_BodemPrmtrsN    = 6; {-Bodem-gerelateerde parameters: Natuur}
  cTB_MBP             = 7; {-Tabel met milieu-belasting-punten (MBP)  }
  cTB_SpuitSchemas    = 8; {-Spuitschema's}

  {-Mapping of EP[ indx-1 ]: xdep-Table numbering}
  cTS_LandgebruikType = 0; {-LandgebruikType}
  cTS_BodemType       = 1; {-BodemType}
  cTS_SpuitSchemas    = 2; {-Geselecteerde spuitschema (-)}
  cTS_CalcType        = 3; {-CalcType: (0=Av, 1=Min, 2=Max)}

  {-LandgebruikType-codes}
  cd_akkerbouw = 1;
  cd_bos       = 2;
  cd_gras      = 3;
  cd_natuur    = 4;
  MaxLandgebr  = 4;

  {-BodemType-codes: zie 'dsmodel 109 tabellen.xls'}

  {-Model specifieke fout-codes}
  cInvld_LandgebruikType = -9800;
  cInvld_BodemType       = -9801;
  cInvld_SpuitSchema     = -9802;
  cInvld_CalcType        = -9803;
  cAantal_MBP_onbekend   = -9804;

var
  Indx: Integer; {-Index of Boot-procedure used. Must be set by boot-procedure!}
  {-Als verschillende TBootEPArray-functies de 'mapping' beinvloeden van
    de externe parameters op de EPArray, dan kan deze variabele binnen de
    Speed-procedures worden benut om toch de gewenste gegevens te vinden}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
				   (zie nDC) }
  {-Min/Max values of key-values: must be set by boot-procedure!}
  cMinLandgebruikType, cMaxLandgebruikType,
  cMinBodemType,       cMaxBodemType,
  cMinSpuitSchema,     cMaxSpuitSchema,
  cMinCalcType,        cMaxCalcType: Integer;
Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
	if ( nDC > 0 ) then
      ModelProfile.Free;
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Returns the derivatives dydx at location x, given, x, the function values y
  and the external parameters EP. IErr=0 if no error occured during the
  calculation of dydx}
Type
  TBodRelDat = Record
    OMpercAv,          {Av.org.matter perc. bouwvoor (%)}
    OMpercMin,         {Min.org.matter perc. bouwvoor (%)}
    OMpercMax: Double; {Max.org.matter perc. bouwvoor (%)}
  end;
  TCalcType = (Av, Min, Max);
  TApplType = (Voorjaar, Najaar);
var
  LandgebruikType, BodemType, SpuitSchema: Integer; {-key-values}
  CalcType: TCalcType;                              {-key-value}
  LGT,            {-Landgebr.type waarbij bodemtype-geg. is gevonden}
  RecNr: Integer; {-Recordnr. idem}
  BodRelDat: TBodRelDat;
  MBPtot: Double;
  i: Integer;
Const
  cNietSpuiten = 0;

Function SetKeyValues( var IErr: Integer ): Boolean;
var
  hlp: Integer;

Function SetBodRelDat( const BodemType, LandgebruikType: Integer;
         var LGT,  {-Landgebr.type waarbij geg. is gevonden}
         RecNr,{-Record idem }
         IErr: Integer ): Boolean;
const  cSearchOrderArray: array[1..MaxLandgebr] of Integer =
                         (cd_gras, cd_akkerbouw, cd_bos, cd_natuur);
var
  i: Integer;
Function FindAndFillRecord( const BodemType, LandgebruikType: Integer;
                            var RecNr: Integer ): Boolean;
var  NRows, BT: Integer;
Function GetBodRelTableNr( const LandgebruikType: Integer ): Integer;
begin
  case LandgebruikType of
  cd_akkerbouw : Result := cTb_BodemPrmtrsA;
  cd_bos       : Result := cTb_BodemPrmtrsB;
  cd_natuur    : Result := cTb_BodemPrmtrsN;
  else
    Result := cTb_BodemPrmtrsG; {-Gras}
  end;
end; {-Function GetBodRelTableNr}
Procedure FillBodRelDat( const LandgebruikType, RecNr: Integer );
begin
  with EP[ cEP0 ].xInDep.Items[ GetBodRelTableNr( LandgebruikType ) ] do begin
    with BodRelDat do begin
      OMpercAv     := GetValue( RecNr, 2 );
      OMpercMin    := GetValue( RecNr, 3 );
      OMpercMax    := GetValue( RecNr, 4 );
    end;
  end;
end; {-Procedure FillBodRelDat}
begin {-Function FindAndFillRecord}
  Result := False;
  RecNr  := 0;
  with EP[ cEP0 ].xInDep.Items[ GetBodRelTableNr( LandgebruikType ) ] do begin
    NRows := GetNRows;
    repeat
      Inc( RecNr );
      BT := BodemType - 1;
      if ( RecNr <= NRows ) then begin
        BT     := Trunc( GetValue( RecNr, 1 ) );
        Result := ( BodemType = BT );
        if Result then
          FillBodRelDat( LandgebruikType, RecNr );
      end;
    until ( Result or ( BT > BodemType ) or ( RecNr = NRows ) );
  end; {with}
end; {-Function FindAndFillRecord}

begin {-Function SetBodRelDat}
  {-Probeer eerst de gegevens te vinden in de tabel van het gespecificeerde
    landgebruik-type}
  IErr   := cNoError;
  Result := False;
  LGT    := LandgebruikType;

  if FindAndFillRecord( BodemType, LGT, RecNr ) then begin
    Result := True; Exit;
  end;
  {-Als dit niet is gelukt, probeer dan de tabellen met de andere landgebruiks-
    typen, waarbij de volgorde van cSearchOrderArray wordt gehandhaafd.}
  for i:=1 to MaxLandgebr do begin
    LGT := cSearchOrderArray[ i ];
    if ( LGT <> LandgebruikType ) then begin
      if FindAndFillRecord( BodemType, LGT, RecNr ) then begin
        Result := true; Exit;
      end;
    end;
  end;
  {-Het gespecificeerde bodemtype is niet aangetroffen}
  IErr := cInvld_BodemType;
end; {-Function SetBodRelDat}

begin {-Function SetKeyValues}
  Result := False;
  with EP[ indx-1 ].xDep do begin {-Value of indx MUST be set by boot-procedure}
    SpuitSchema := Trunc( Items[ cTS_SpuitSchemas ].EstimateY( x, Direction ) );
    if ( SpuitSchema < cMinSpuitSchema ) or ( SpuitSchema > cMaxSpuitSchema ) then begin
      IErr := cInvld_SpuitSchema; Exit;
    end;
    if ( SpuitSchema = cNietSpuiten ) then begin {-Doe geen moeite als er niet}
      Result := True; Exit;                      { gespoten wordt}
    end;
    LandgebruikType := Trunc( Items[ cTS_LandgebruikType ].EstimateY( x, Direction ) );
    if ( LandgebruikType < cMinLandgebruikType ) or ( LandgebruikType > cMaxLandgebruikType ) then begin
      IErr := cInvld_LandgebruikType; Exit;
    end;
    BodemType := Trunc( Items[ cTS_BodemType ].EstimateY( x, Direction ) );
    if ( BodemType < cMinBodemType ) or ( BodemType > cMaxBodemType ) then begin
      IErr := cInvld_BodemType; Exit;
    end;
    hlp := Trunc( Items[ cTS_CalcType ].EstimateY( x, Direction ) );
    if ( hlp < cMinCalcType ) or ( hlp > cMaxCalcType ) then begin
      IErr := cInvld_CalcType; Exit;
    end;
    CalcType := TCalcType( hlp );
    if ( not SetBodRelDat( BodemType, LandgebruikType, LGT, RecNr, IErr ) ) then
      Exit;
  end;
  Result := True;
end; {-Function SetKeyValues}

Function Get_MBPtot( var MBPtot: Double; var IErr: Integer ): Boolean;
Type
  TSpuitSchemaRec = Record
    SpuitSchema,
    PestType: Integer;
    PestGift: Double;
    ApplType: TApplType;
  end;
var
  SpuitSchemaRec: TSpuitSchemaRec;
  NRows, i: Integer;
  TableValue: Double;
Procedure SetSpuitSchemaRec( const RecNr: Integer );
begin
  with EP[ cEP0 ].xInDep.Items[ cTB_SpuitSchemas ] do
    with SpuitSchemaRec do begin
      SpuitSchema := Trunc ( GetValue( RecNr, 1 ) );
      PestType    := Trunc ( GetValue( RecNr, 2 ) );
      PestGift    :=         GetValue( RecNr, 3 );
      ApplType    := TApplType( Trunc ( GetValue( RecNr, 4 ) ) );
    end;
end; {-Procedure SetSpuitSchemaRec}
Function OMPerc( const CalcType: TCalcType ): Double;
begin
  Case CalcType of
    Min:  Result := BodRelDat.OMpercMax;
    Max:  Result := BodRelDat.OMpercMin;
  else
    Result := BodRelDat.OMpercAv;
  end;
end; {-Function OMPerc}
Function GetMBPfromTable( const PestType: Integer; const OMPerc: Double;
                          ApplType: TApplType ): Double;
var
  iColOffset: Integer;
begin
  if ( OMPerc < 1.5 ) then
    iColOffset := 1
  else if ( OMPerc < 3 ) then
    iColOffset := 3
  else if ( OMPerc < 6 ) then
    iColOffset := 5
  else if ( OMPerc < 12 ) then
    iColOffset := 7
  else
    iColOffset := 9;
  with EP[ cEP0 ].xInDep.Items[ cTB_MBP ] do
    Result := GetValue( PestType, iColOffset + Integer( ApplType ) );
end; {-Function GetMBPfromTable}
begin
  IErr   := cNoError;
  Result := False;
  MBPtot := 0;
  if ( SpuitSchema = cNietSpuiten ) then begin {-Doe geen moeite als er niet}
    Result := True; Exit;                      { gespoten wordt}
  end;
  NRows := EP[ cEP0 ].xInDep.Items[ cTB_SpuitSchemas ].GetNRows;
  i     := 0;
  Repeat
    Inc( i );
    if ( i <= NRows ) then begin
      SetSpuitSchemaRec( i );
      if ( SpuitSchema = SpuitSchemaRec.SpuitSchema ) then begin
        with SpuitSchemaRec do begin
		  TableValue := GetMBPfromTable( PestType, OMPerc( CalcType ), ApplType );
		  if ( TableValue >= 0 ) then
            MBPtot := MBPtot + PestGift * TableValue
		  else begin
		    MBPtot := 0; Result := False; IErr := Trunc( TableValue ); Exit;
		  end;
		end;
        Result := True;
      end;
    end;
  Until ( ( SpuitSchema < SpuitSchemaRec.SpuitSchema ) or ( i = NRows ) );
  if ( not Result ) then
    IErr := cInvld_SpuitSchema;
end; {-Function Get_MBPtot}

begin

  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;

  if ( Context = UpdateYstart ) then begin
    {-*** Override initial values on ystart-vector here}
    {-*** END Override initial values on ystart-vector here}

    {-Converteer dag-waarden uit tijdreeksen en invoertijdstippen afkomstig van
      de Shell naar jaren}
    if ( indx = cBoot2 ) then
      ScaleTimesFromShell( cFromDayToYear, EP );

    IErr := cNoError;
  end else begin             {-Fill dydx-vector}
    {$ifdef test}
    Application.MessageBox( 'SetKeyValues', 'Info', MB_OKCANCEL );
    {$endif}

    if not SetKeyValues( IErr ) then
      Exit;

    if Get_MBPtot( MBPtot, IErr ) then
      dydx[ cMBP ] := MBPtot
    else
      dydx[ cMBP ] := 0;

  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables (PestType, PestFlx, LandgebruikType, BodemType,
    Neerslagoverschot (q), L) are NOT set by this boot-procedure: they have to be
    initialised in another way}
Procedure SetMinMaxKeyValues;
begin
  with EP[ cEP0 ].xInDep.Items[ cTb_MinMaxValKeys ] do begin
    cMinLandgebruikType    := Trunc( GetValue( 1, 1 ) );
    cMaxLandgebruikType    := Trunc( GetValue( 1, 2 ) );
    cMinBodemType          := Trunc( GetValue( 1, 3 ) );
    cMaxBodemType          := Trunc( GetValue( 1, 4 ) );
    cMinSpuitSchema        := Trunc( GetValue( 1, 5 ) );
    cMaxSpuitSchema        := Trunc( GetValue( 1, 6 ) );
    cMinCalcType           := Trunc( GetValue( 1, 7 ) );
    cMaxCalcType           := Trunc( GetValue( 1, 8 ) );
  end;
end;
Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC,
            cNrXIndepTblsInEP0, cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then
    SetMinMaxKeyValues;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Apart from the defaults for TestBootEP, this procedure also sets the
    xDep-tables, so the model is ready-to-run }
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then Exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx,
                                           EP );
  if ( Result <> cNoError ) then Exit;
  SetReadyToRun( EP);
  {$ifdef test}
  Application.MessageBox( 'ReadyToRun', 'Info', MB_OKCANCEL );
  {$endif}
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables are NOT set by this boot-procedure: they must be supplied
    by the shell }
begin
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}
begin
  {-This 'DLL-Main-block' is executed  when the DLL is initially loaded into
    memory (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );  
end.
