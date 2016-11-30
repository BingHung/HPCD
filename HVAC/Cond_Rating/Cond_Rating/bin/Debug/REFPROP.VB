

' class for  function prototypes in VB.NET
Imports System.Runtime.InteropServices
Public Module RefProp
    <DllImport("Refprop.dll", EntryPoint:="SETUPdll", SetLastError:=True, _
 CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
Setupdll(ByRef nc As Int32, ByVal hfld As String, ByVal hfmix As String, ByVal hrf As String, ByRef ierr As Int32, _
     <Out()> ByVal herr As Char(), ByRef ln1 As Int32, ByRef ln2 As Int32, ByRef ln3 As Int32, ByRef ln4 As Int32) As Integer
    End Function

    <DllImport("Refprop.dll", EntryPoint:="SETMODdll", SetLastError:=True, _
 CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SetModdll(ByRef nc As Int32, ByVal htype As String, ByVal hmix As String, ByVal hcomp As String, ByRef ierr As Int32, _
     <Out()> ByVal herr As Char(), ByRef ln1 As Int32, ByRef ln2 As Int32, ByRef ln3 As Int32, ByRef ln4 As Int32) As Integer
    End Function

    <DllImport("Refprop.dll", EntryPoint:="SETREFdll", SetLastError:=True, _
 CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SetRefdll(ByVal hrf As String, ByRef ixflag As Int32, ByRef x0 As Double, ByRef h0 As Double, ByRef s0 As Double, _
ByRef t0 As Double, ByRef p0 As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln1 As Int32, ByRef ln2 As Int32) As Integer
    End Function

    <DllImport("Refprop.dll", EntryPoint:="SETKTVdll", SetLastError:=True, _
 CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SetKTVdll(ByRef i As Int32, ByRef j As Int32, ByVal hmodij As String, ByVal fij As Double, ByVal hmix As String, ByRef ierr As Int32, _
     <Out()> ByVal herr As Char(), ByRef ln1 As Int32, ByRef ln2 As Int32, ByRef ln3 As Int32) As Integer

    End Function

    <DllImport("Refprop.dll", EntryPoint:="GETKTVdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
GetKTVdll(ByRef icomp As Int32, ByRef jcomp As Int32, <Out()> ByVal hmodij As String, <Out()> ByVal fij As Double, _
<Out()> ByVal hmix As String, <Out()> ByVal hfij() As String, <Out()> ByVal hbinp As String, <Out()> ByVal hmxrul As String, _
ByRef ln1 As Int32, ByRef ln2 As Int32, ByRef ln3 As Int32, ByRef ln4 As Int32, ByRef ln5 As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="INFOdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
INFOdll(ByRef icomp As Int32, <Out()> ByRef wmm As Double, <Out()> ByRef ttp As Double, <Out()> ByRef tnbpt As Double, _
<Out()> ByRef tc As Double, <Out()> ByRef pc As Double, <Out()> ByRef dc As Double, <Out()> ByRef Zc As Double, _
<Out()> ByRef acf As Double, <Out()> ByRef dip As Double, <Out()> ByRef Rgas As Double) As Integer

    End Function

    <DllImport("Refprop.dll", EntryPoint:="SATTdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SATTdll(ByRef t As Double, ByRef x As Double, ByRef KPH As Int32, <Out()> ByRef p As Double, _
<Out()> ByRef RHOL As Double, <Out()> ByRef RHOV As Double, <Out()> ByRef xliq As Double, _
<Out()> ByRef xvap As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function

    <DllImport("Refprop.dll", EntryPoint:="SATPdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SATPdll(ByRef p As Double, ByRef x As Double, ByRef KPH As Int32, <Out()> ByRef TK As Double, _
<Out()> ByRef RHOF As Double, <Out()> ByRef RHOG As Double, <Out()> ByRef xliq As Double, _
<Out()> ByRef xvap As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="SATDdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SATDdll(ByRef rho As Double, ByRef x As Double, ByRef KPH As Int32, <Out()> ByRef kr As Double, _
<Out()> ByRef t As Double, <Out()> ByRef p As Double, <Out()> ByRef RHOL As Double, <Out()> ByRef RHOV As Double, _
<Out()> ByRef xliq As Double, <Out()> ByRef xvap As Double, ByRef ierr As Int32, _
<Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function

    <DllImport("Refprop.dll", EntryPoint:="SATHdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SATHdll(ByRef h As Double, ByRef x As Double, ByRef KPH As Int32, <Out()> ByRef nroot As Double, _
<Out()> ByRef k1 As Double, <Out()> ByRef t1 As Double, <Out()> ByRef p1 As Double, <Out()> ByRef d1 As Double, _
<Out()> ByRef k2 As Double, <Out()> ByRef t2 As Double, <Out()> ByRef p2 As Double, <Out()> ByRef d2 As Double, ByRef ierr As Int32, _
<Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="SATSdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
SATSdll(ByRef s As Double, ByRef x As Double, ByRef KPH As Int32, <Out()> ByRef nroot As Double, _
<Out()> ByRef k1 As Double, <Out()> ByRef t1 As Double, <Out()> ByRef p1 As Double, <Out()> ByRef d1 As Double, _
<Out()> ByRef k2 As Double, <Out()> ByRef t2 As Double, <Out()> ByRef p2 As Double, <Out()> ByRef d2 As Double, _
<Out()> ByRef k3 As Double, <Out()> ByRef t3 As Double, <Out()> ByRef p3 As Double, <Out()> ByRef d3 As Double, _
ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function

    <DllImport("Refprop.dll", EntryPoint:="TPFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
TPFLSHdll(ByRef t As Double, ByRef p As Double, ByRef z As Double, <Out()> ByRef D As Double, _
<Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, <Out()> ByRef x As Double, _
<Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef h As Double, <Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function

    <DllImport("Refprop.dll", EntryPoint:="TDFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
TDFLSHdll(ByRef t As Double, ByRef d As Double, ByRef z As Double, <Out()> ByRef p As Double, _
<Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, <Out()> ByRef x As Double, _
<Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef h As Double, <Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="PDFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
PDFLSHdll(ByRef p As Double, ByRef D As Double, ByRef z As Double, <Out()> ByRef t As Double, _
<Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, <Out()> ByRef x As Double, _
<Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef h As Double, <Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="PHFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
PHFLSHdll(ByRef p As Double, ByRef h As Double, ByRef z As Double, <Out()> ByRef t As Double, <Out()> ByRef D As Double, _
<Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, <Out()> ByRef x As Double, _
<Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="PSFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
PSFLSHdll(ByRef p As Double, ByRef s As Double, ByRef z As Double, <Out()> ByRef t As Double, <Out()> ByRef D As Double, _
<Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, <Out()> ByRef x As Double, _
<Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef h As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="PEFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
PEFLSHdll(ByRef p As Double, ByRef e As Double, ByRef z As Double, <Out()> ByRef t As Double, <Out()> ByRef D As Double, _
<Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, <Out()> ByRef x As Double, _
<Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef h As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="THFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
THFLSHdll(ByRef t As Double, ByRef h As Double, ByRef z As Double, ByRef kr As Int32, _
<Out()> ByRef p As Double, <Out()> ByRef D As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer
        'Often in the liquid, two solutions exist, one of them in the two phase.
        ' If this is the case, call THFLSH with kr=2 to get the single-phase state.
    End Function
    <DllImport("Refprop.dll", EntryPoint:="TSFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
TSFLSHdll(ByRef t As Double, ByRef s As Double, ByRef z As Double, ByRef kr As Int32, _
<Out()> ByRef p As Double, <Out()> ByRef D As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef h As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="TEFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
TEFLSHdll(ByRef t As Double, ByRef e As Double, ByRef z As Double, ByRef kr As Int32, _
<Out()> ByRef p As Double, <Out()> ByRef D As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef h As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="DHFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
DHFLSHdll(ByRef D As Double, ByRef h As Double, ByRef z As Double, <Out()> ByRef t As Double, _
<Out()> ByRef p As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="DSFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
DSFLSHdll(ByRef D As Double, ByRef s As Double, ByRef z As Double, <Out()> ByRef t As Double, _
<Out()> ByRef p As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef h As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="DEFLSHdll", SetLastError:=True, _
   CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
DEFLSHdll(ByRef D As Double, ByRef e As Double, ByRef z As Double, <Out()> ByRef t As Double, _
<Out()> ByRef p As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef h As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer
    End Function
    <DllImport("Refprop.dll", EntryPoint:="HSFLSHdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
HSFLSHdll(ByRef h As Double, ByRef s As Double, ByRef z As Double, ByRef t As Double, _
<Out()> ByRef p As Double, <Out()> ByRef D As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef q As Double, <Out()> ByRef e As Double, _
<Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="TQFLSHdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
TQFLSHdll(ByRef t As Double, ByRef q As Double, ByRef z As Double, ByRef kq As Int32, _
<Out()> ByRef p As Double, <Out()> ByRef D As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef e As Double, <Out()> ByRef h As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="PQFLSHdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
PQFLSHdll(ByRef p As Double, ByRef q As Double, ByRef z As Double, ByRef kq As Int32, _
<Out()> ByRef t As Double, <Out()> ByRef D As Double, <Out()> ByRef D1 As Double, <Out()> ByRef Dv As Double, _
<Out()> ByRef x As Double, <Out()> ByRef y As Double, <Out()> ByRef e As Double, <Out()> ByRef h As Double, _
<Out()> ByRef s As Double, <Out()> ByRef Cv As Double, <Out()> ByRef Cp As Double, _
<Out()> ByRef w As Double, ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function
    <DllImport("Refprop.dll", EntryPoint:="TPRHOdll", SetLastError:=True, _
CharSet:=CharSet.Ansi, ExactSpelling:=True, _
CallingConvention:=CallingConvention.StdCall)> _
Public Function _
TPRHOdll(ByRef t As Double, ByRef p As Double, ByRef z As Double, ByRef kph As Int32, _
ByRef kguess As Int32, <Out()> ByRef rho As Double, _
ByRef ierr As Int32, <Out()> ByVal herr As Char(), ByRef ln As Int32) As Integer

    End Function

    'An environment variable can be set to specify the locations of the fluid files.  This can
    'be done under Start/Settings/Control Panel/System/Advanced/Environment Variables.  A new
    'variable called RPPrefix should be created with a value of "C:\Program Files\Refprop" (or
    'the directory where Refprop is located).  The Refprop FAQ page shows pictures of these variables.

    'Alternatively:
    'Change the following to the location of the fluid files and of the mixture files (those with .MIX).
    'If the file Refprop.DLL is not located in the application workspace, then see note below.
    'DO NOT IMPLEMENT BOTH OF THESE METHODS, change either the paths below or modify the environment variables, not both.

    Private Const FluidsDirectory As String = "fluids\"
    Private Const MixturesDirectory As String = "mixtures\"
    '----Usually the files are located here, so comment out the two lines above and use these two:
    'Private Const FluidsDirectory As String = "c:\ProgramFiles\REFPROP\fluids\"
    'Private Const MixturesDirectory As String = "c:\ProgramFiles\REFPROP\mixtures\"

    'REFPROP Excel Functions (See the file "REFPROP with Instructions and Help.XLS" for more details.)
    'Arguments:  (FluidName, InpCode, Units, Prop1, Prop2)
    'FluidName = text,  fluid must be either in Fluids or Mixtures sub directories.
    'InpCode   = name and order of Prop1 and Prop2.
    '            "TP" would mean Prop1 is Temperature, Prop2 is Pressure (need quotes)
    '            Valid InpCodes:  TP,TD,TH,TS,TE,TQ,PD,PH,PS,PE,PQ,DH,DS,DE,HS
    '            To define saturated liquid or vapor inputs:  TLIQ, TVAP, PLIQ, PVAP
    '            Other:  Crit, Trip, TMelt, PMelt, TSubl, PSubl
    '
    '            The word "Optional" appears in the argument listings below to indicate that
    '            PROP2 is not always required depending on the InpCode argument.
    '            In some cases, PROP1 is also optional.
    '
    'Units     = "SI", "SI with C" (or just "C"), "Molar SI", "E", "molar E", "cgs", "mks", "M" (need quotes).  "SI" is used by default if no input is given. (Unless DefaultUnits changed in VBA code)
    'Prop1     = numerical value of the first input property (in the units of the previous line)
    'Prop2     = numerical value of the second input property (if required).

    'Function Temperature           (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Pressure              (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Density               (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function CompressibilityFactor (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function LiquidDensity         (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function VaporDensity          (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Volume                (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Energy                (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Enthalpy              (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Entropy               (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function IsochoricHeatCapacity (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Cv                    (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function IsobaricHeatCapacity  (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Cp                    (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Csat                  (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function SpeedOfSound          (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Sound                 (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Quality               (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function LatentHeat            (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function HeatOfVaporization    (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function HeatOfCombustion      (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function GrossHeatingValue     (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function NetHeatingValue       (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function JouleThomson          (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function IsentropicExpansionCoef       (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function IsothermalCompressibility     (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function VolumeExpansivity             (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function AdiabaticCompressibility      (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function AdiabaticBulkModulus          (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function IsothermalExpansionCoef       (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function IsothermalBulkModulus         (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function SecondVirial          (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Viscosity             (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function ThermalConductivity   (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function Prandtl               (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function SurfaceTension        (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function DielectricConstant    (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function MolarMass             (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function EOSMax                (FluidName, InpCode, Units, Prop1, Optional Prop2)
    'Function EOSMin                (FluidName, InpCode, Units, Prop1, Optional Prop2)

    'In the following, i is the component number in the mixture (where the maximum value of i can be 20)
    'Function MoleFraction          (FluidName, i)
    'Function MassFraction          (FluidName, i)
    'Function LiquidMoleFraction    (FluidName, Optional InpCode, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 as double = Nothing, Optional ByVal Prop2 as double = Nothing, Optional i)
    'Function VaporMoleFraction     (FluidName, Optional InpCode, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 as double = Nothing, Optional ByVal Prop2 as double = Nothing, Optional i)
    'Function Mole2Mass(FluidName, i, Prop1, Prop2, Optional Prop3, Optional Prop4, Optional Prop5, Optional Prop6, Optional Prop7, Optional Prop8, Optional Prop9, Optional Prop10, Optional Prop11, Optional Prop12, Optional Prop13, Optional Prop14, Optional Prop15, Optional Prop16, Optional Prop17, Optional Prop18, Optional Prop19, Optional Prop20)
    'Function Mass2Mole(FluidName, i, Prop1, Prop2, Optional Prop3, Optional Prop4, Optional Prop5, Optional Prop6, Optional Prop7, Optional Prop8, Optional Prop9, Optional Prop10, Optional Prop11, Optional Prop12, Optional Prop13, Optional Prop14, Optional Prop15, Optional Prop16, Optional Prop17, Optional Prop18, Optional Prop19, Optional Prop20)



    'In order for Excel to access the Refprop.DLL file, you will need to do one of the items below:
    '  -  Place your Excel file in the Refprop directory
    '  -  Change the path and environment variables as described at the top of the "Examples" sheet
    '  -  Below, replace all "Refprop.DLL" with "C:\Program Files\Refprop\Refprop.DLL" (or the subdirectory where Refprop was installed)
    '  -  Place a copy of Refprop.DLL in your directory where your Excel files are located (not preferred)
    Private Const MaxComps As Integer = 20
    Public Declare Sub SETMIXdll Lib "REFPROP.DLL" (ByVal hmxnme As String, ByVal hfmix As String, ByVal hrf As String, ByRef ncc As Integer, ByVal hfile As String, ByRef x As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln1 As Integer, ByRef ln2 As Integer, ByRef ln3 As Integer, ByRef ln4 As Integer, ByRef ln5 As Integer)
    'Public Declare Sub GERG04dll Lib "REFPROP.DLL" (byref nc as integer, byref iflag as integer, byref ierr as integer, ByVal herr As Char(), byref ln1 as integer)
    Public Declare Sub THERM2dll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef p As Double, ByRef e As Double, ByRef h As Double, ByRef s As Double, ByRef cv As Double, ByRef cp As Double, ByRef w As Double, ByRef z As Double, ByRef hjt As Double, ByRef aH As Double, ByRef g As Double, ByRef kappa As Double, ByRef beta As Double, ByRef dPdD_T As Double, ByRef d2PdD2_rho As Double, ByRef dPdT_rho As Double, ByRef dDdT_P As Double, ByRef dDdP_rho As Double, ByRef spare1 As Double, ByRef spare2 As Double, ByRef spare3 As Double, ByRef spare4 As Double)
    Public Declare Sub THERM3dll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef kappa As Double, ByRef beta As Double, ByRef isenk As Double, ByRef kt As Double, ByRef betas As Double, ByRef bs As Double, ByRef kkt As Double, ByRef thrott As Double, ByRef pi As Double, ByRef spht As Double)

    Public Declare Sub THERMdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef p As Double, ByRef e As Double, ByRef h As Double, ByRef s As Double, ByRef cv As Double, ByRef cp As Double, ByRef w As Double, ByRef hjt As Double)
    Public Declare Sub THERM0dll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef p As Double, ByRef e As Double, ByRef h As Double, ByRef s As Double, ByRef cv As Double, ByRef cp As Double, ByRef w As Double, ByRef a As Double, ByRef g As Double)
    Public Declare Sub ENTROdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef s As Double)
    Public Declare Sub ENTHALdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef h As Double)
    Public Declare Sub CVCPdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef cv As Double, ByRef cp As Double)
    Public Declare Sub PRESSdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef p As Double)
    Public Declare Sub AGdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef a As Double, ByRef g As Double)
    Public Declare Sub HEATdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rho As Double, ByRef x As Double, ByRef hg As Double, ByRef hn As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)

    Public Declare Sub DPDDdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rho As Double, ByRef x As Double, ByRef dPdD_T As Double)
    Public Declare Sub DPDD2dll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rho As Double, ByRef x As Double, ByRef d2PdD2_rho As Double)
    Public Declare Sub DPDTdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rho As Double, ByRef x As Double, ByRef dPdT_rho As Double)
    Public Declare Sub DDDPdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rho As Double, ByRef x As Double, ByRef dDdP_rho As Double)
    Public Declare Sub DDDTdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rho As Double, ByRef x As Double, ByRef dDdT_P As Double)
    Public Declare Sub DHD1dll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rho As Double, ByRef x As Double, ByRef dHdT_D As Double, ByRef dHdT_P As Double, ByRef dHdD_T As Double, ByRef dHdD_P As Double, ByRef dHdP_T As Double, ByRef dHdP_D As Double)

    Public Declare Sub SATDdll Lib "REFPROP.DLL" (ByRef d As Double, ByRef x As Double, ByRef kph As Integer, ByRef kr As Integer, ByRef t As Double, ByRef p As Double, ByRef Dl As Double, ByRef Dv As Double, ByRef xliq As Double, ByRef xvap As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub SATHdll Lib "REFPROP.DLL" (ByRef h As Double, ByRef x As Double, ByRef kph As Integer, ByRef nroot As Integer, ByRef k1 As Integer, ByRef t1 As Double, ByRef p1 As Double, ByRef d1 As Double, ByRef k2 As Integer, ByRef t2 As Double, ByRef p2 As Double, ByRef d2 As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub SATEdll Lib "REFPROP.DLL" (ByRef e As Double, ByRef x As Double, ByRef kph As Integer, ByRef nroot As Integer, ByRef k1 As Integer, ByRef t1 As Double, ByRef p1 As Double, ByRef d1 As Double, ByRef k2 As Integer, ByRef t2 As Double, ByRef p2 As Double, ByRef d2 As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub SATSdll Lib "REFPROP.DLL" (ByRef s As Double, ByRef x As Double, ByRef kph As Integer, ByRef nroot As Integer, ByRef k1 As Integer, ByRef t1 As Double, ByRef p1 As Double, ByRef d1 As Double, ByRef k2 As Integer, ByRef t2 As Double, ByRef p2 As Double, ByRef d2 As Double, ByRef k3 As Integer, ByRef t3 As Double, ByRef p3 As Double, ByRef d3 As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub CV2PKdll Lib "REFPROP.DLL" (ByRef icomp As Integer, ByRef t As Double, ByRef rho As Double, ByRef cv2p As Double, ByRef csat As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub CSATKdll Lib "REFPROP.DLL" (ByRef icomp As Integer, ByRef t As Double, ByRef kph As Integer, ByRef p As Double, ByRef rho As Double, ByRef csat As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub DPTSATKdll Lib "REFPROP.DLL" (ByRef icomp As Integer, ByRef t As Double, ByRef kph As Integer, ByRef p As Double, ByRef rho As Double, ByRef csat As Double, ByRef dpdtsat As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)

    Public Declare Sub ESFLSHdll Lib "REFPROP.DLL" (ByRef e As Double, ByRef s As Double, ByRef z As Double, ByRef t As Double, ByRef p As Double, ByRef d As Double, ByRef Dl As Double, ByRef Dv As Double, ByRef xliq As Double, ByRef xvap As Double, ByRef q As Double, ByRef h As Double, ByRef cv As Double, ByRef cp As Double, ByRef w As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub CCRITdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef p As Double, ByRef v As Double, ByRef x As Double, ByRef cs As Double, ByRef ts As Double, ByRef Ds As Double, ByRef ps As Double, ByRef ws As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub CSTARdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef p As Double, ByRef v As Double, ByRef x As Double, ByRef cs As Double, ByRef ts As Double, ByRef Ds As Double, ByRef ps As Double, ByRef ws As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub FPVdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef p As Double, ByRef x As Double, ByRef f As Double)
    'Public declare Sub SPECGRdll Lib "REFPROP.DLL" (t As Double, d As Double, p As Double, Gr As Double)

    Public Declare Sub ABFL1dll Lib "REFPROP.DLL" (ByRef a As Double, ByRef b As Double, ByRef x As Double, ByRef i As Integer, ByVal ab As String, ByRef dmin As Double, ByRef dmax As Double, ByRef t As Double, ByRef p As Double, ByRef d As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln1 As Integer, ByRef ln2 As Integer)
    Public Declare Sub ABFL2dll Lib "REFPROP.DLL" (ByRef a As Double, ByRef b As Double, ByRef x As Double, ByRef kq As Integer, ByRef ksat As Integer, ByVal ab As String, ByRef tbub As Double, ByRef tdew As Double, ByRef pbub As Double, ByRef pdew As Double, ByRef Dlbub As Double, ByRef Dvdew As Double, ByRef ybub As Double, ByRef xdew As Double, ByRef t As Double, ByRef p As Double, ByRef Dl As Double, ByRef Dv As Double, ByRef x As Double, ByRef y As Double, ByRef q As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer, ByRef ln2 As Integer)
    Public Declare Sub DBFL2dll Lib "REFPROP.DLL" (ByRef d As Double, ByRef b As Double, ByRef x As Double, ByRef i As Integer, ByVal ab As String, ByRef t As Double, ByRef p As Double, ByRef Dl As Double, ByRef Dv As Double, ByRef xliq As Double, ByRef xvap As Double, ByRef q As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer, ByRef ln2 As Integer)

    Public Declare Sub CRITPdll Lib "REFPROP.DLL" (ByRef x As Double, ByRef tc As Double, ByRef pc As Double, ByRef dc As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub VIRBdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef x As Double, ByRef b As Double)
    Public Declare Sub DBDTdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef x As Double, ByRef dbt As Double)
    Public Declare Sub VIRCdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef x As Double, ByRef c As Double)
    Public Declare Sub TRNPRPdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef eta As Double, ByRef tcx As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub FGCTYdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef f As Double)
    Public Declare Sub DIELECdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef de As Double)
    Public Declare Sub SURFTdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef d As Double, ByRef x As Double, ByRef sigma As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub SURTENdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef rhol As Double, ByRef rhov As Double, ByRef xl As Double, ByRef xv As Double, ByRef sigma As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub MELTTdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef x As Double, ByRef p As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub MLTH2Odll Lib "REFPROP.DLL" (ByRef t As Double, ByRef p1 As Double, ByRef p2 As Double)
    Public Declare Sub MELTPdll Lib "REFPROP.DLL" (ByRef p As Double, ByRef x As Double, ByRef t As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub SUBLTdll Lib "REFPROP.DLL" (ByRef t As Double, ByRef x As Double, ByRef p As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub SUBLPdll Lib "REFPROP.DLL" (ByRef p As Double, ByRef x As Double, ByRef t As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)

    Public Declare Sub WMOLdll Lib "REFPROP.DLL" (ByRef x As Double, ByRef wm As Double)
    Public Declare Sub XMASSdll Lib "REFPROP.DLL" (ByRef xmol As Double, ByRef xkg As Double, ByRef wmix As Double)
    Public Declare Sub XMOLEdll Lib "REFPROP.DLL" (ByRef xkg As Double, ByRef xmol As Double, ByRef wmix As Double)
    Public Declare Sub QMASSdll Lib "REFPROP.DLL" (ByRef qmol As Double, ByRef xl As Double, ByRef xv As Double, ByRef qkg As Double, ByRef xlkg As Double, ByRef xvkg As Double, ByRef wliq As Double, ByRef wvap As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    Public Declare Sub QMOLEdll Lib "REFPROP.DLL" (ByRef qkg As Double, ByRef xlkg As Double, ByRef xvkg As Double, ByRef qmol As Double, ByRef xl As Double, ByRef xv As Double, ByRef wliq As Double, ByRef wvap As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln As Integer)
    'Public Declare Sub INFOdll Lib "REFPROP.DLL" (byref icomp As Integer, byref wmm As Double, byref ttrp As Double, byref tnbpt As Double, byref tc As Double, byref pc As Double, byref dc As Double, byref Zc As Double, byref acf As Double, byref dip As Double, byref Rgas As Double)
    Public Declare Sub LIMITXdll Lib "REFPROP.DLL" (ByVal htyp As String, ByRef t As Double, ByRef d As Double, ByRef p As Double, ByRef x As Double, ByRef tmin As Double, ByRef tmax As Double, ByRef dmax As Double, ByRef pmax As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln1 As Integer, ByRef ln2 As Integer)
    Public Declare Sub LIMITKdll Lib "REFPROP.DLL" (ByVal htyp As String, ByRef icomp As Integer, ByRef t As Double, ByRef d As Double, ByRef p As Double, ByRef tmin As Double, ByRef tmax As Double, ByRef dmax As Double, ByRef pmax As Double, ByRef ierr As Integer, ByVal herr As Char(), ByRef ln1 As Integer, ByRef ln2 As Integer)

    Public Declare Sub GETKTVdll Lib "REFPROP.DLL" (ByRef icomp As Integer, ByRef jcomp As Integer, ByVal hmodij As String, ByRef fij As Double, ByVal hfmix As String, ByVal hfij As String, ByVal hbinp As String, ByVal hmxrul As String, ByRef ln1 As Integer, ByRef ln2 As Integer, ByRef ln3 As Integer, ByRef ln4 As Integer, ByRef ln5 As Integer)
    Public Declare Sub GETFIJdll Lib "REFPROP.DLL" (ByVal hmodij As String, ByRef fij As Double, ByVal hfij As String, ByVal hmxrul As String, ByRef ln1 As Integer, ByRef ln2 As Integer, ByRef ln3 As Integer)
    Public Declare Sub PREOSdll Lib "REFPROP.DLL" (ByRef i As Integer)
    Public Declare Sub SETAGAdll Lib "REFPROP.DLL" (ByRef ierr As Integer, ByVal herr As Char(), ByRef ln1 As Integer)
    Public Declare Sub PUREFLDdll Lib "REFPROP.DLL" (ByRef icomp As Integer)

    'Used to call Refprop:
    Dim herr(255) As Char
    Dim herr2(255) As Char
    Dim hfmix As String     '(255) As Char
    Dim hfmix2 As String    '(255) As Char
    Dim hrf As String       '(3) As Char
    Dim htyp As String      '(3) As Char
    Dim hmxnme As String    '(255) As Char
    Private hfld As String  '(10000) As Char
    Private nc As Integer, phase As Integer, molmass As Integer
    Private x(MaxComps) As Double, xliq(MaxComps) As Double, xvap(MaxComps) As Double, xmm(MaxComps) As Double, xkg(MaxComps) As Double, xmol(MaxComps) As Double, wmix As Double
    Private ierr As Int32, ierr2 As Integer, kq As Integer, kr As Integer
    Private t As Double, p As Double, d As Double, Dl As Double, Dv As Double, q As Double, wm As Double, tz As Double, pz As Double, dz As Double, dd As Double
    Private e As Double, h As Double, s As Double, Cvcalc As Double, Cpcalc As Double, w As Double
    Private tmin As Double, tmax As Double, dmax As Double, pmax As Double
    Private tc As Double, pc As Double, dc As Double
    Private tbub As Double, tdew As Double, pbub As Double, pdew As Double, Dlbub As Double, Dvdew As Double, ybub(MaxComps) As Double, xdew(MaxComps) As Double
    Private eta As Double, tcx As Double, sigma As Double, hjt As Double, de As Double
    Private wmm As Double, ttrp As Double, tnbpt As Double, Zc As Double, acf As Double, dip As Double, Rgas As Double
    Private tUnits(10) As String, taUnits(10) As String, pUnits(10) As String, dUnits(10) As String, vUnits(10) As String, hUnits(10) As String, sUnits(10) As String, wUnits(10) As String, visUnits(10) As String, tcxUnits(10) As String, stUnits(10) As String
    Private tUnits2 As String = "", taUnits2 As String, pUnits2 As String, dUnits2 As String, vUnits2 As String, hUnits2 As String, sUnits2 As String, wUnits2 As String, visUnits2 As String, tcxUnits2 As String, stUnits2 As String
    Private FldOld As String
    Private z As Double, aHelm As Double, Gibbs As Double, xkappa As Double, beta As Double
    Private dPdD_T As Double, d2PdD2_rho As Double, dPdT_rho As Double, dDdT_P As Double, dDdP_rho As Double
    Private spare1 As Double, spare2 As Double, spare3 As Double, spare4 As Double
    Private isenk As Double, kt As Double, betas As Double, bs As Double, kkt As Double, thrott As Double, xpi As Double, spht As Double
    Private nroot As Integer, k1 As Integer, k2 As Integer, k3 As Integer, t2 As Double, p2 As Double, d2 As Double, t3 As Double, p3 As Double, d3 As Double

    Private Const CtoK = 273.15                  'Exact conversion
    Private Const FtoR = 459.67                  'Exact conversion
    Private Const RtoK = 5 / 9                   'Exact conversion
    Private Const HtoS = 3600                    'Exact conversion
    Private Const ATMtoMPa = 0.101325            'Exact conversion
    Private Const BARtoMPA = 0.1                 'Exact conversion
    Private Const KGFtoN = 98.0665 / 10          'Exact conversion
    Private Const INtoM = 0.0254                 'Exact conversion
    Private Const FTtoM = 12 * INtoM             'Exact conversion
    Private Const LBMtoKG = 0.45359237           'Exact conversion
    Private Const CALtoJ = 4.184                 'Exact conversion (tc)
    'private Const CALtoJ = 4.1868                'Exact conversion (IT) (Use this one only with pure water)
    Private Const MMHGtoMPA = ATMtoMPa / 760     'Exact conversion
    Private Const INH2OtoMPA = 0.000249082

    Private Const BTUtoKJ = CALtoJ * LBMtoKG * RtoK
    Private Const LBFtoN = LBMtoKG * KGFtoN
    Private Const IN3toM3 = INtoM * INtoM * INtoM
    Private Const FT3toM3 = FTtoM * FTtoM * FTtoM
    Private Const GALLONtoM3 = IN3toM3 * 231
    Private Const PSIAtoMPA = LBMtoKG / INtoM / INtoM * KGFtoN / 1000000
    Private Const FTLBFtoJ = FTtoM * LBFtoN
    Private Const HPtoW = 550 * FTLBFtoJ
    Private Const BTUtoW = BTUtoKJ * 1000
    Private Const LBFTtoNM = LBFtoN / FTtoM
    Private CompFlag As Integer

    Function Setup(ByRef FluidName As String)
        Dim a As String, ab As String, FluidNme As String, FlNme As String
        Dim i As Integer, sum As Double, sc As Integer, ncc As Integer, nc2 As Integer, mass As Integer
        Dim hRef As Double, sRef As Double, Tref As Double, pref As Double
        Dim htype As String = Space(3), hmix As String = Space(3), hcomp As String = Space(60)
        Dim RPPrefix As String, FluidsPrefix As String, MixturesPrefix As String
        Dim xtemp(MaxComps) As Double

        Setup = Nothing
        ierr = 0
        FlNme = FluidName
        If InStr(FluidName, "error") Then Exit Function
        If InStr(FluidName, "Inputs are out of range") Then Exit Function
        If FluidName = FldOld Then Exit Function
        FldOld = ""
        Call CheckName(FluidName)

        RPPrefix = Trim(Environ("RPPrefix"))
        If Right(RPPrefix, 1) = "\" Then RPPrefix = Left(RPPrefix, Len(RPPrefix) - 1)

        If RPPrefix = "" Then
            FluidsPrefix = My.Application.Info.DirectoryPath & "\fluids\"  'FluidsDirectory
            MixturesPrefix = My.Application.Info.DirectoryPath & "\mixtures\"  ' MixturesDirectory
            'FluidsPrefix = FluidsDirectory
            'MixturesPrefix = MixturesDirectory
        Else
            FluidsPrefix = RPPrefix & "\" & FluidsDirectory
            MixturesPrefix = RPPrefix & "\" & MixturesDirectory
        End If

        hrf = "DEF"
        hfmix = FluidsPrefix & "hmx.bnc"

        On Error GoTo ErrorHandler
        'ChDrive(Application.ActiveWorkbook.Path)
        'ChDir(Application.ActiveWorkbook.Path)
        On Error GoTo 0

        a = ""
        For i = 0 To MaxComps : xtemp(i) = 0 : Next
        mass = 0

        'Set up the model

        If InStr(UCase(FluidName), ".MIX") Then
            'Open MixturesPrefix & FluidName For Input As #1
            'Line Input #1, ab
            'Line Input #1, ab
            'Input #1, nc2
            'For i = 0 To nc2
            '  Line Input #1, ab
            '  a = a & FluidsPrefix & ab & "|"
            'Next
            'For i = 0 To nc2
            '  Input #1, xtemp(i)
            'Next
            'Close 1
            'hfld = a
            hmxnme = MixturesPrefix & FluidName
            Call SETMIXdll(hmxnme, hfmix, hrf, nc2, hfld, xtemp(0), ierr, herr, 255, 255, 3, 10000, 255)

        ElseIf InStr(FluidName, ",") Or InStr(FluidName, ";") Then
            FluidNme = Trim(FluidName)
            If InStr(FluidNme, ";") Then sc = 1 Else sc = 0
            If UCase(Right(FluidNme, 4)) = "MASS" Then mass = 1 : FluidNme = Trim(Left(FluidNme, Len(FluidNme) - 4))
            nc2 = 0
            Do
                If sc = 0 Then i = InStr(FluidNme, ",") Else i = InStr(FluidNme, ";")
                If i = 0 Then i = Len(FluidNme) + 1
                nc2 = nc2 + 1
                If nc2 > MaxComps Then ierr = 1 : herr = Trim2("Too many components") : Exit Function
                ab = Trim(Left(FluidNme, i - 1))
                Call CheckName(ab)
                If InStr(LCase(ab), ".fld") = 0 Then ab = ab + ".fld"
                a = a & FluidsPrefix & ab & "|"
                FluidNme = Mid(FluidNme, i + 1)
                If sc = 0 Then i = InStr(FluidNme, ",") Else i = InStr(FluidNme, ";")
                If i = 0 Then i = Len(FluidNme) + 1
                xtemp(nc2 - 1) = CDbl(Left(FluidNme, i - 1))
                FluidNme = Trim(Mid(FluidNme, i + 1))
            Loop Until FluidNme = ""
            sum = 0
            For i = 0 To nc2 : sum = sum + xtemp(i) : Next
            If sum <= 0 Then ierr = 1 : herr = Trim2("Composition not set") : Exit Function
            For i = 0 To nc2 : xtemp(i) = xtemp(i) / sum : Next
            hfld = a
            If nc2 < 1 Then ierr = 1 : herr = Trim2("Setup failed") : Exit Function
            'To load the GERG-2004 pure fluid equations of state rather than the defaults
            'that come with Refprop, call the GERG04dll routine with a 1 as the second input.
            '(Note that there is one more occurrence of this below)
            'Call GERG04dll(nc2, 1, ierr, herr, 255)

            Setupdll(nc2, hfld, hfmix, hrf, ierr, herr, 10000, 255, 3, 255)

            'Setupdll(nc, HFLD_st, HFMIX_st, HRF_st, ierr, HERR_ch, _
            '    refpropfluidpathlength, refpropcharlength, _
            '    lengthofreference, errormessagelength)


            'To load the AGA8 equation of state, call SETAGA after calling SETUP
            '(Note that there is one more occurrence of this below)
            'Call SETAGAdll(ierr, herr, 255)
        ElseIf InStr(FluidName, "/") <> 0 And InStr(FluidName, "(") <> 0 Then
            FluidNme = Trim(FluidName)
            If UCase(Right(FluidNme, 4)) = "MASS" Then mass = 1 : FluidNme = Trim(Left(FluidNme, Len(FluidNme) - 4))
            nc2 = 0
            Do
                i = InStr(FluidNme, "/")
                If InStr(FluidNme, "(") < i Then i = InStr(FluidNme, "(")
                If i = 0 Then i = Len(FluidNme) + 1
                nc2 = nc2 + 1
                If nc2 > MaxComps Then ierr = 1 : herr = Trim2("Too many components") : Exit Function
                ab = Trim(Left(FluidNme, i - 1))
                Call CheckName(ab)
                If InStr(LCase(ab), ".fld") = 0 Then ab = ab + ".fld"
                a = a & FluidsPrefix & ab & "|"
                FluidNme = Trim(Mid(FluidNme, i))
                If Left(FluidNme, 1) = "/" Then FluidNme = Trim(Mid(FluidNme, 2))
            Loop Until Left(FluidNme, 1) = "("
            FluidNme = Mid(FluidNme, 2)
            If Right(FluidNme, 1) = ")" Then FluidNme = Trim(Left(FluidNme, Len(FluidNme) - 1))
            ncc = 0
            Do
                i = InStr(FluidNme, "/")
                If i = 0 Then i = Len(FluidNme) + 1
                ncc = ncc + 1
                If ncc > MaxComps Then ierr = 1 : herr = Trim2("Too many components") : Exit Function
                xtemp(ncc) = CDbl(Left(FluidNme, i - 1))
                FluidNme = Mid(FluidNme, i + 1)
            Loop Until FluidNme = ""
            sum = 0
            For i = 0 To nc2 : sum = sum + xtemp(i) : Next
            If sum <= 0 Then ierr = 1 : herr = Trim2("Composition not set") : Exit Function
            For i = 0 To nc2 : xtemp(i) = xtemp(i) / sum : Next
            hfld = a
            If nc2 < 1 Then ierr = 1 : herr = Trim2("Setup failed") : Exit Function
            'To load the GERG-2004 pure fluid equations of state rather than the defaults
            'that come with Refprop, call the GERG04dll routine with a 1 as the second input.
            '(Note that there is one more occurrence of this above)
            'Call GERG04dll(nc2, 1, ierr, herr, 255)

            Call Setupdll(nc2, hfld, hfmix, hrf, ierr, herr, 10000, 255, 3, 255)

            'To load the AGA8 equation of state, call SETAGA after calling SETUP
            'Call SETAGAdll(ierr, herr, 255)
        Else
            nc2 = 1
            If InStr(LCase(FluidName), ".fld") = 0 And InStr(LCase(FluidName), ".ppf") = 0 Then FluidName = FluidName + ".fld"
            If InStr(FluidName, "\") Then
                hfld = FluidName
            Else
                hfld = FluidsPrefix & FluidName
            End If

            '...Use call to SETMOD to change the equation of state for any of the
            '.....pure components from the default (recommended) values.
            '.....This should only be implemented by an experienced user.
            'If InStr(LCase(hfld), "argon") <> 0 And nc2 = 1 Then
            '  hcomp = "FE1": htype = "EOS": hmix = hcomp
            '  Call SETMODdll(nc2, htype, hmix, hcomp, ierr, herr, 3, 3, 60, 255)
            'End If

            Call Setupdll(nc2, hfld, hfmix, hrf, ierr, herr, 10000, 255, 3, 255)
        End If

        'Use call to PREOSdll to change the equation of state to Peng Robinson for all calculations.
        'To revert back to the normal REFPROP EOS and models, use:  Call PREOSdll(0)
        'Call PREOSdll(2)

        If mass Then
            For i = 0 To nc2
                xkg(i) = xtemp(i)
            Next
            Call XMOLEdll(xkg(0), xtemp(0), wmix)
        End If

        If ierr <= 0 Then
            nc = nc2           'If setup was successful, load new values of nc and x()
            For i = 0 To nc
                x(i) = xtemp(i)
            Next
            Setup = FluidName
            FldOld = FlNme

            'Use the following line to calculate enthalpies and entropies on a reference state
            'based on the currently defined mixture, or to change to some other reference state.
            'The routine does not have to be called, but doing so will cause calculations
            'to be the same as those produced from the graphical interface for mixtures.
            Call SetRefdll(hrf, 2, x(0), hRef, sRef, Tref, pref, ierr, herr, 3, 255)
        Else
            Setup = Trim2(herr)
            FldOld = ""
        End If
        Exit Function

ErrorHandler:
        Resume Next
    End Function

    Sub CheckName(ByRef FluidName As String)
Restart:
        If Left(FluidName, 1) = Chr(34) Then
            FluidName = Mid(FluidName, 2) : GoTo Restart
        End If
        If Right(FluidName, 1) = Chr(34) Then
            FluidName = Left(FluidName, Len(FluidName) - 1) : GoTo Restart
        End If
        If UCase(FluidName) = "AIR" Then FluidName = "nitrogen;7812;argon;0092;oxygen;2096"
        If UCase(FluidName) = "CARBON DIOXIDE" Then
            FluidName = "CO2"
        ElseIf UCase(FluidName) = "CO2" Then
            FluidName = "CO2"
        End If
        If UCase(FluidName) = "CARBON MONOXIDE" Then
            FluidName = "CO"
        ElseIf UCase(FluidName) = "CO" Then
            FluidName = "CO"
        End If
        If UCase(FluidName) = "CARBONYL SULFIDE" Then FluidName = "COS"
        If UCase(FluidName) = "CYCLOHEXANE" Then FluidName = "CYCLOHEX"
        If UCase(FluidName) = "CYCLOPROPANE" Then FluidName = "CYCLOPRO"
        If UCase(FluidName) = "DEUTERIUM" Then FluidName = "D2"
        If UCase(FluidName) = "HEAVY WATER" Then FluidName = "D2O"
        If UCase(FluidName) = "HYDROGEN SULFIDE" Then FluidName = "H2S"
        If UCase(FluidName) = "IBUTANE" Then FluidName = "ISOBUTAN"
        If UCase(FluidName) = "ISOBUTANE" Then FluidName = "ISOBUTAN"
        If UCase(FluidName) = "ISOPENTANE" Then FluidName = "IPENTANE"
        If UCase(FluidName) = "NEOPENTANE" Then FluidName = "NEOPENTN"
        If UCase(FluidName) = "ISOHEXANE" Then FluidName = "IHEXANE"
        If UCase(FluidName) = "NITROUS OXIDE" Then FluidName = "N2O"
        If UCase(FluidName) = "PARAHYDROGEN" Then FluidName = "PARAHYD"
        If UCase(FluidName) = "PROPYLENE" Then FluidName = "PROPYLEN"
        If UCase(FluidName) = "SULFUR HEXAFLUORIDE" Then FluidName = "SF6"
        If UCase(FluidName) = "DIMETHYLETHER" Then FluidName = "DME"
        If UCase(FluidName) = "BUTENE" Then FluidName = "1BUTENE"
        If UCase(FluidName) = "ISOBUTENE" Then FluidName = "IBUTENE"
        If UCase(FluidName) = "CIS-BUTENE" Then FluidName = "C2BUTENE"
        If UCase(FluidName) = "TRANS-BUTENE" Then FluidName = "T2BUTENE"
        If UCase(FluidName) = "TRIFLUOROIODOMETHANE" Then FluidName = "CF3I"
        If UCase(FluidName) = "PERFLUOROBUTANE" Then FluidName = "C4F10"
        If UCase(FluidName) = "PERFLUOROPENTANE" Then FluidName = "C5F12"
        If UCase(FluidName) = "NITROGEN TRIFLUORIDE" Then FluidName = "NF3"
        If UCase(FluidName) = "SULFUR DIOXIDE" Then FluidName = "SO2"
        If UCase(FluidName) = "SULFUR HEXAFLUORIDE" Then FluidName = "SF6"
        If UCase(FluidName) = "DODECANE" Then FluidName = "C12"
        If UCase(FluidName) = "DIMETHYLETHER" Then FluidName = "DME"
        If UCase(FluidName) = "WATER" Then FluidName = "hydrogen;0.7;oxygen;0.3"
        If UCase(FluidName) = "AR" Then FluidName = "argon"
        If FluidName = "N2" Then FluidName = "nitrogen"
        If FluidName = "H2" Then FluidName = "hydrogen"
        If FluidName = "O2" Then FluidName = "oxygen"

    End Sub

    Sub CalcSetup(ByRef FluidName As String, Optional ByRef InpCode As String = Nothing, Optional ByRef Units As String = Nothing, Optional ByRef prop1 As Double = Nothing, Optional ByRef prop2 As Double = Nothing)
        Call Setup(FluidName)
        If ierr > 0 Then Exit Sub
        Call ConvertUnitsRefProp(InpCode, Units, prop1, prop2)
        'herr = ""
        q = 0 : t = 0 : p = 0 : d = 0 : Dl = 0 : Dv = 0 : e = 0 : h = 0 : s = 0 : Cvcalc = 0 : Cpcalc = 0 : w = 0
    End Sub

    Sub CalcProp(ByRef FluidName As String, ByRef InpCode As String, ByVal Units As String, Optional ByRef prop1 As Double = Nothing, Optional ByRef prop2 As Double = Nothing)
        Dim iflag1 As Integer, iflag2 As Integer
        'ThisWorkbook.Activate()
        q = 0 : t = 0 : p = 0 : d = 0 : Dl = 0 : Dv = 0 : e = 0 : h = 0 : s = 0 : Cvcalc = 0 : Cpcalc = 0 : w = 0

        If IsNothing(prop1) Then iflag1 = 1
        If iflag1 = 0 Then
            If Len(Trim(prop1)) = 0 Then iflag1 = 2
            If iflag1 = 0 Then If CDbl(prop1) = 0 And prop1 <> "0" Then ierr = 1 : herr = Trim2("Invalid input: ") + prop1 : Exit Sub
        End If

        If IsNothing(prop2) Then iflag2 = 1
        If iflag2 = 0 Then
            If Len(Trim(prop2)) = 0 Then iflag2 = 2
            If iflag2 = 0 Then If CDbl(prop2) = 0 And prop2 <> "0" Then ierr = 1 : herr = Trim2("Invalid input: ") + prop2 : Exit Sub
        End If

        If InpCode = Nothing Then InpCode = ""
        Call CalcSetup(FluidName, InpCode, Units, prop1, prop2)
        If UCase(Left(InpCode, 4)) = "CRIT" Then
            Call CRITPdll(x(0), t, p, d, ierr, herr, 255)
            If ierr = 0 Then Call THERMdll(t, d, x(0), pc, e, h, s, Cvcalc, Cpcalc, w, hjt)
            Exit Sub
        ElseIf UCase(Left(InpCode, 4)) = "TRIP" Then
            If nc <> 1 Then ierr = 1 : herr = Trim2("Can only return triple point for a pure fluid") : Exit Sub
            Call INFOdll(1, wmm, t, tnbpt, tc, pc, dc, Zc, acf, dip, Rgas)
            Call SATTdll(t, x(0), 1, p, d, Dv, xliq(0), xvap(0), ierr, herr, 255)
            If ierr = 0 Then Call THERMdll(t, d, x(0), pc, e, h, s, Cvcalc, Cpcalc, w, hjt)
            Exit Sub
        End If

        If iflag1 Then ierr = 1 : herr = Trim2("Inputs are missing") : Exit Sub
        If ierr > 0 Then Exit Sub
        If InpCode <> "" Then Call Calc(InpCode, prop1, prop2, iflag1, iflag2)
    End Sub

    Sub Calc(ByVal InputCode As Object, ByVal prop1 As Double, ByVal prop2 As Double, ByVal iflag1 As Object, ByVal iflag2 As Object)
        Dim a As String, Input1 As String, Input2 As String, InpCode As String, i As Integer, pp As Double
        Dim xlkg(MaxComps) As Double, xvkg(MaxComps) As Double, xlj(MaxComps) As Double, xvj(MaxComps) As Double, qmol As Double, wliq As Double, wvap As Double
        ierr = 0
        InpCode = InputCode.ToString.ToUpper.Trim
        Input2 = ""
        Input1 = Left(InpCode, 1)
        If Len(InpCode) = 2 Then Input2 = Mid(InpCode, 2, 1)
        If Len(InpCode) = 3 Then
            a = Right(InpCode, 1)
            If a = "&" Or a = "<" Or a = ">" Then Input2 = Mid(InpCode, 2, 1)
        End If
        If Left(InpCode, 2) = "TP" Or Left(InpCode, 2) = "PT" Then Input2 = Mid(InpCode, 2, 1)

        If Input1 = "T" Then t = prop1 : If iflag1 >= 1 Then GoTo Error1
        If Input1 = "P" Then p = prop1 : If iflag1 >= 1 Then GoTo Error1
        If Input1 = "D" Then d = prop1 : If iflag1 >= 1 Then GoTo Error1
        If Input1 = "V" And prop1 <> 0 And Len(InpCode) = 2 Then d = 1 / prop1 : Mid(InpCode, 1, 1) = "D" : If iflag1 >= 1 Then GoTo Error1
        If Input1 = "E" Then e = prop1 : If iflag1 >= 1 Then GoTo Error1
        If Input1 = "H" Then h = prop1 : If iflag1 >= 1 Then GoTo Error1
        If Input1 = "S" Then s = prop1 : If iflag1 >= 1 Then GoTo Error1
        If Input1 = "Q" Then q = prop1 : If iflag1 >= 1 Then GoTo Error1
        If Input2 = "T" Then t = prop2 : If iflag2 >= 1 Then GoTo Error2
        If Input2 = "P" Then p = prop2 : If iflag2 >= 1 Then GoTo Error2
        If Input2 = "D" Then d = prop2 : If iflag2 >= 1 Then GoTo Error2
        If Input2 = "V" And prop2 <> 0 And Len(InpCode) = 2 Then d = 1 / prop2 : Mid(InpCode, 2, 1) = "D" : If iflag2 >= 1 Then GoTo Error2
        If Input2 = "E" Then e = prop2 : If iflag2 >= 1 Then GoTo Error2
        If Input2 = "H" Then h = prop2 : If iflag2 >= 1 Then GoTo Error2
        If Input2 = "S" Then s = prop2 : If iflag2 >= 1 Then GoTo Error2
        If Input2 = "Q" Then q = prop2 : If iflag2 >= 1 Then GoTo Error2

        phase = 2
        If Len(InpCode) > 1 Then If UCase(Mid(InpCode, 2, 1)) = "L" Then phase = 1

        For i = 0 To nc
            xliq(i) = 0 : xvap(i) = 0
        Next
        If Left(InpCode, 1) = "T" And t <= 0 Then herr = Trim2("Input temperature is zero") : Exit Sub
        'Calculate saturation values given temperature
        If InpCode = "TL" Or InpCode = "TLIQ" Or InpCode = "TVAP" Then
            Call SATTdll(t, x(0), phase, p, Dl, Dv, xliq(0), xvap(0), ierr, herr, 255)
            If (p = 0 Or Dl = 0) And ierr = 0 Then ierr = 1 : herr = Trim2("Inputs are out of range") : Exit Sub
            d = Dl : q = 0
            If phase = 2 Then d = Dv : q = 1
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            'Calculate saturation values given pressure
        ElseIf InpCode = "PL" Or InpCode = "PLIQ" Or InpCode = "PVAP" Then
            Call SATPdll(p, x(0), phase, t, Dl, Dv, xliq(0), xvap(0), ierr, herr, 255)
            If (p = 0 Or Dl = 0) And ierr = 0 Then ierr = 1 : herr = Trim2("Inputs are out of range") : Exit Sub
            d = Dl : q = 0
            If phase = 2 Then d = Dv : q = 1
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            'Calculate saturation values given density
        ElseIf InpCode = "DSAT" Or InpCode = "DL" Or InpCode = "DLIQ" Or InpCode = "DVAP" Then
            Call SATDdll(d, x(0), 1, kr, t, p, Dl, Dv, xliq(0), xvap(0), ierr, herr, 255)
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            q = kr - 1
            'Calculate propeties given the enthalpy along the saturation line
        ElseIf InpCode = "HSAT" Or InpCode = "HSAT1" Then
            Call SATHdll(h, x(0), 0, nroot, k1, t, p, d, k2, t2, p2, d2, ierr, herr, 255)
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            q = k1 - 1
            If nroot = 0 Then ierr = 1 : herr = Trim2("Inputs are out of range") : Exit Sub
            'Calculate propeties given the enthalpy along the saturation line for the second possible root
        ElseIf InpCode = "HSAT2" Then
            Call SATHdll(h, x(0), 0, nroot, k2, t2, p2, d2, k1, t, p, d, ierr, herr, 255)
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            q = k1 - 1
            If nroot < 2 Then ierr = 1 : herr = Trim2("Inputs are out of range or there is no second root") : Exit Sub
            'Calculate propeties given the entropy along the saturation line
        ElseIf InpCode = "SSAT" Or InpCode = "SSAT1" Then
            Call SATSdll(s, x(0), 0, nroot, k1, t, p, d, k2, t2, p2, d2, k3, t3, p3, d3, ierr, herr, 255)
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            q = k1 - 1
            If nroot = 0 Then ierr = 1 : herr = Trim2("Inputs are out of range") : Exit Sub
            'Calculate propeties given the entropy along the saturation line for the second possible root
        ElseIf InpCode = "SSAT2" Then
            Call SATSdll(s, x(0), 0, nroot, k2, t2, p2, d2, k1, t, p, d, k3, t3, p3, d3, ierr, herr, 255)
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            q = k1 - 1
            If nroot < 2 Then ierr = 1 : herr = Trim2("Inputs are out of range or there is no second root") : Exit Sub
            'Calculate propeties given the entropy along the saturation line for the third possible root
        ElseIf InpCode = "SSAT3" Then
            Call SATSdll(s, x(0), 0, nroot, k3, t3, p3, d3, k2, t2, p2, d2, k1, t, p, d, ierr, herr, 255)
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            q = k1 - 1
            If nroot < 3 Then ierr = 1 : herr = Trim2("Inputs are out of range or there is no third root") : Exit Sub
        ElseIf InpCode = "TPL" Or InpCode = "PTL" Then
            Call TPRHOdll(t, p, x(0), 1, 0, d, ierr, herr, 255)
            Dl = d : Dv = d : q = 990
            Call THERMdll(t, d, x(0), pp, e, h, s, Cvcalc, Cpcalc, w, hjt)
        ElseIf InpCode = "TPV" Or InpCode = "PTV" Then
            Call TPRHOdll(t, p, x(0), 2, 0, d, ierr, herr, 255)
            Dl = d : Dv = d : q = 990
            Call THERMdll(t, d, x(0), pp, e, h, s, Cvcalc, Cpcalc, w, hjt)
        ElseIf InpCode = "TP" Or InpCode = "PT" Then
            Call TPFLSHdll(t, p, x(0), d, Dl, Dv, xliq(0), xvap(0), q, e, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "TD" Or InpCode = "DT" Then
            Call TDFLSHdll(t, d, x(0), p, Dl, Dv, xliq(0), xvap(0), q, e, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "TD&" Or InpCode = "DT&" Then
            'Do not perform any flash calculation here
            Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
            q = 990
        ElseIf Left(InpCode, 2) = "TH" Or Left(InpCode, 2) = "HT" Then
            If InStr(InpCode, "<") Then
                Call THFLSHdll(t, h, x(0), 1, p, d, Dl, Dv, xliq(0), xvap(0), q, e, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
            Else
                Call THFLSHdll(t, h, x(0), 2, p, d, Dl, Dv, xliq(0), xvap(0), q, e, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
            End If
        ElseIf InpCode = "TS" Or InpCode = "ST" Then
            Call TSFLSHdll(t, s, x(0), 1, p, d, Dl, Dv, xliq(0), xvap(0), q, e, h, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "TE" Or InpCode = "ET" Then
            Call TEFLSHdll(t, e, x(0), 2, p, d, Dl, Dv, xliq(0), xvap(0), q, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "TQ" Or InpCode = "QT" Then
            Call TQFLSHdll(t, q, x(0), molmass, p, d, Dl, Dv, xliq(0), xvap(0), e, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
            If molmass = 2 Then
                Call XMASSdll(xliq(0), xlkg(0), wmix)
                Call XMASSdll(xvap(0), xvkg(0), wmix)
                Call QMOLEdll(q, xlkg(0), xvkg(0), qmol, xlj(0), xvj(0), wliq, wvap, ierr, herr, 255)
                q = qmol
            End If
        ElseIf InpCode = "PD" Or InpCode = "DP" Then
            Call PDFLSHdll(p, d, x(0), t, Dl, Dv, xliq(0), xvap(0), q, e, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "PH" Or InpCode = "HP" Then
            Call PHFLSHdll(p, h, x(0), t, d, Dl, Dv, xliq(0), xvap(0), q, e, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "PS" Or InpCode = "SP" Then
            Call PSFLSHdll(p, s, x(0), t, d, Dl, Dv, xliq(0), xvap(0), q, e, h, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "PE" Or InpCode = "EP" Then
            Call PEFLSHdll(p, e, x(0), t, d, Dl, Dv, xliq(0), xvap(0), q, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "PQ" Or InpCode = "QP" Then
            Call PQFLSHdll(p, q, x(0), molmass, t, d, Dl, Dv, xliq(0), xvap(0), e, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
            If molmass = 2 Then
                Call XMASSdll(xliq(0), xlkg(0), wmix)
                Call XMASSdll(xvap(0), xvkg(0), wmix)
                Call QMOLEdll(q, xlkg(0), xvkg(0), qmol, xlj(0), xvj(0), wliq, wvap, ierr, herr, 255)
                q = qmol
            End If
        ElseIf InpCode = "DH" Or InpCode = "HD" Then
            Call DHFLSHdll(d, h, x(0), t, p, Dl, Dv, xliq(0), xvap(0), q, e, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "DS" Or InpCode = "SD" Then
            Call DSFLSHdll(d, s, x(0), t, p, Dl, Dv, xliq(0), xvap(0), q, e, h, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "DE" Or InpCode = "ED" Then
            Call DEFLSHdll(d, e, x(0), t, p, Dl, Dv, xliq(0), xvap(0), q, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "HS" Or InpCode = "SH" Then
            Call HSFLSHdll(h, s, x(0), t, p, d, Dl, Dv, xliq(0), xvap(0), q, e, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "TMELT" Then
            Call MELTTdll(t, x(0), p, ierr, herr, 255)
            If ierr = 0 Then Call TPFLSHdll(t, p, x(0), d, Dl, Dv, xliq(0), xvap(0), q, e, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "PMELT" Then
            If p = 0 Then ierr = 1 : herr = Trim2("Input pressure is zero") : Exit Sub
            Call MELTPdll(p, x(0), t, ierr, herr, 255)
            If ierr = 0 Then Call TPFLSHdll(t, p, x(0), d, Dl, Dv, xliq(0), xvap(0), q, e, h, s, Cvcalc, Cpcalc, w, ierr, herr, 255)
        ElseIf InpCode = "TSUBL" Then
            Call SUBLTdll(t, x(0), p, ierr, herr, 255)
            If ierr = 0 And p = 0 Then ierr = 1 : herr = Trim2("No sublimation line available")
            If ierr = 0 Then
                q = 1
                d = p / 8.314472 / t
                Call TPRHOdll(t, p, x(0), 2, 1, d, ierr, herr, 255)
                Call THERMdll(t, d, x(0), pp, e, h, s, Cvcalc, Cpcalc, w, hjt)
            End If
        ElseIf InpCode = "PSUBL" Then
            If p = 0 Then ierr = 1 : herr = Trim2("Input pressure is zero") : Exit Sub
            Call SUBLPdll(p, x(0), t, ierr, herr, 255)
            If ierr = 0 And t = 0 Then ierr = 1 : herr = Trim2("No sublimation line available")
            If ierr = 0 Then
                q = 1
                d = p / 8.314472 / t
                Call TPRHOdll(t, p, x(0), 2, 1, d, ierr, herr, 255)
                Call THERMdll(t, d, x(0), pp, e, h, s, Cvcalc, Cpcalc, w, hjt)
            End If
        Else
            ierr = 1 : herr = Trim2("Invalid input code")
        End If
        If (q <= 0.000001 Or q >= 0.999999) And Cvcalc = -9999980 Then Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
        Exit Sub

Error1:
        ierr = 1 : herr = Trim2("First property missing") : Exit Sub
Error2:
        ierr = 1 : herr = Trim2("Second property missing") : Exit Sub

    End Sub

    Function Temperature(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Temperature = ConvertUnitsRefProp("-T", Units, t, 0)
    End Function

    Function Pressure(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Pressure = ConvertUnitsRefProp("-P", Units, p, 0)
    End Function

    Function Density(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Density = ConvertUnitsRefProp("-D", Units, d, 0)
    End Function

    Function CompressibilityFactor(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call INFOdll(1, wmm, ttrp, tnbpt, tc, pc, dc, Zc, acf, dip, Rgas)
        CompressibilityFactor = p / d / t / Rgas
    End Function

    Function LiquidDensity(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr <> 0 Then LiquidDensity = Trim2(herr) : Exit Function
        If q < 0 Or q > 1 Then
            LiquidDensity = Trim2("Inputs are single phase")
        Else
            CompFlag = 1
            LiquidDensity = ConvertUnitsRefProp("-D", Units, Dl, 0)
            CompFlag = 0
        End If
    End Function

    Function VaporDensity(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr <> 0 Then VaporDensity = Trim2(herr) : Exit Function
        If q < 0 Or q > 1 Then
            VaporDensity = Trim2("Inputs are single phase")
        Else
            CompFlag = 2
            VaporDensity = ConvertUnitsRefProp("-D", Units, Dv, 0)
            CompFlag = 0
        End If
    End Function

    Function Volume(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim v As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Volume = 0
        If d <= 0 Then Volume = Trim2("Density is zero") : Exit Function
        v = 1 / d
        Volume = ConvertUnitsRefProp("-V", Units, v, 0)
    End Function

    Function Energy(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Energy = ConvertUnitsRefProp("-H", Units, e, 0)
    End Function

    Function Enthalpy(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Enthalpy = ConvertUnitsRefProp("-H", Units, h, 0)
    End Function

    Function Entropy(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Entropy = ConvertUnitsRefProp("-S", Units, s, 0)
    End Function

    Function IsochoricHeatCapacity(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        IsochoricHeatCapacity = ConvertUnitsRefProp("-S", Units, Cvcalc, 0)
    End Function

    Function cv(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        cv = ConvertUnitsRefProp("-S", Units, Cvcalc, 0)
    End Function

    Function IsobaricHeatCapacity(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        IsobaricHeatCapacity = ConvertUnitsRefProp("-S", Units, Cpcalc, 0)
    End Function

    Function cp(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        cp = ConvertUnitsRefProp("-S", Units, Cpcalc, 0)
    End Function

    Function csat(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        csat = Nothing
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If nc <> 1 Then csat = "Csat can only be called for a pure fluid" : Exit Function
        Call CSATKdll(1, t, 1, p, d, csat, ierr, herr, 255)
        csat = ConvertUnitsRefProp("-S", Units, csat, 0)
    End Function

    Function SpeedOfSound(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        SpeedOfSound = ConvertUnitsRefProp("-W", Units, w, 0)
    End Function

    Function Sound(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Sound = ConvertUnitsRefProp("-W", Units, w, 0)
    End Function

    Function LatentHeat(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim hl As Double, hv As Double
        InpCode = Trim(UCase(InpCode))
        If Left(InpCode, 1) = "T" Then
            Call CalcSetup(FluidName, "T", Units, Prop1, Prop2)
            If ierr <> 0 Then LatentHeat = Trim2(herr) : Exit Function
            If nc <> 1 Then LatentHeat = Trim2("Can only be calculated for pure fluids") : Exit Function
            Call INFOdll(1, wmm, ttrp, tnbpt, tc, pc, dc, Zc, acf, dip, Rgas)
            t = Prop1
            If t <= 0 Then LatentHeat = Trim2("Input temperature is zero") : Exit Function
            If t > tc Then LatentHeat = Trim2("Temperature is greater than the critical point temperture") : Exit Function
            Call SATTdll(t, x(0), 1, p, Dl, Dv, xliq(0), xvap(0), ierr, herr, 255)
            If (p = 0 Or Dl = 0) And ierr = 0 Then ierr = 1 : LatentHeat = Trim2("Inputs are out of range") : Exit Function
        ElseIf Left(InpCode, 1) = "P" Then
            Call CalcSetup(FluidName, "P", Units, Prop1, Prop2)
            If ierr <> 0 Then LatentHeat = Trim2(herr) : Exit Function
            If nc <> 1 Then LatentHeat = Trim2("Can only be calculated for pure fluids") : Exit Function
            Call INFOdll(1, wmm, ttrp, tnbpt, tc, pc, dc, Zc, acf, dip, Rgas)
            p = Prop1
            If p <= 0 Then LatentHeat = Trim2("Input pressure is zero") : Exit Function
            If p > pc Then LatentHeat = Trim2("Pressure is greater than the critical point pressure") : Exit Function
            Call SATPdll(p, x(0), 1, t, Dl, Dv, xliq(0), xvap(0), ierr, herr, 255)
            If (t = 0 Or Dl = 0) And ierr = 0 Then ierr = 1 : LatentHeat = Trim2("Inputs are out of range") : Exit Function
        Else
            LatentHeat = Trim2("Valid inputs are only 'T' or 'P'") : Exit Function
        End If
        If ierr <> 0 Then LatentHeat = Trim2(herr) : Exit Function
        Call THERMdll(t, Dl, x(0), p, e, hl, s, Cvcalc, Cpcalc, w, hjt)
        Call THERMdll(t, Dv, x(0), p, e, hv, s, Cvcalc, Cpcalc, w, hjt)
        LatentHeat = ConvertUnitsRefProp("-H", Units, hv - hl, 0)
    End Function

    Function HeatOfVaporization(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        HeatOfVaporization = LatentHeat(FluidName, InpCode, Units, Prop1, Prop2)
    End Function

    Function HeatOfCombustion(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim hg As Double, hn As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call HEATdll(t, d, x(0), hg, hn, ierr, herr, 255)
        HeatOfCombustion = ConvertUnitsRefProp("-H", Units, hg, 0)
    End Function

    Function GrossHeatingValue(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim hg As Double, hn As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call HEATdll(t, d, x(0), hg, hn, ierr, herr, 255)
        GrossHeatingValue = ConvertUnitsRefProp("-H", Units, hg, 0)
    End Function

    Function NetHeatingValue(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim hg As Double, hn As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call HEATdll(t, d, x(0), hg, hn, ierr, herr, 255)
        NetHeatingValue = ConvertUnitsRefProp("-H", Units, hn, 0)
    End Function

    Function JouleThomson(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
        JouleThomson = ConvertUnitsRefProp("-J", Units, hjt, 0)
    End Function

    Function IsentropicExpansionCoef(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM3dll(t, d, x(0), xkappa, beta, isenk, kt, betas, bs, kkt, thrott, xpi, spht)
        IsentropicExpansionCoef = isenk
    End Function

    Function IsothermalCompressibility(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM2dll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, z, hjt, aHelm, Gibbs, xkappa, beta, dPdD_T, d2PdD2_rho, dPdT_rho, dDdT_P, dDdP_rho, spare1, spare2, spare3, spare4)
        IsothermalCompressibility = Trim2("Infinite")
        If d > 1.0E-20 And Not (xkappa = -9999990 Or xkappa > 1.0E+15) Then IsothermalCompressibility = 1 / ConvertUnitsRefProp("-P", Units, 1 / xkappa, 0)
    End Function

    Function VolumeExpansivity(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM2dll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, z, hjt, aHelm, Gibbs, xkappa, beta, dPdD_T, d2PdD2_rho, dPdT_rho, dDdT_P, dDdP_rho, spare1, spare2, spare3, spare4)
        VolumeExpansivity = 1 / ConvertUnitsRefProp("-A", Units, 1 / beta, 0)
    End Function

    Function AdiabaticCompressibility(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM3dll(t, d, x(0), xkappa, beta, isenk, kt, betas, bs, kkt, thrott, xpi, spht)
        AdiabaticCompressibility = Trim2("Infinite")
        If d > 1.0E-20 Then AdiabaticCompressibility = 1 / ConvertUnitsRefProp("-P", Units, 1 / betas, 0)
    End Function

    Function AdiabaticBulkModulus(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM3dll(t, d, x(0), xkappa, beta, isenk, kt, betas, bs, kkt, thrott, xpi, spht)
        AdiabaticBulkModulus = ConvertUnitsRefProp("-P", Units, bs, 0)
    End Function

    Function IsothermalExpansionCoef(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM3dll(t, d, x(0), xkappa, beta, isenk, kt, betas, bs, kkt, thrott, xpi, spht)
        IsothermalExpansionCoef = kt
    End Function

    Function IsothermalBulkModulus(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM3dll(t, d, x(0), xkappa, beta, isenk, kt, betas, bs, kkt, thrott, xpi, spht)
        IsothermalBulkModulus = ConvertUnitsRefProp("-P", Units, kkt, 0)
    End Function

    Function SpecificHeatInput(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM3dll(t, d, x(0), xkappa, beta, isenk, kt, betas, bs, kkt, thrott, xpi, spht)
        SpecificHeatInput = ConvertUnitsRefProp("-H", Units, spht, 0)
    End Function

    Function SecondVirial(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim b As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call VIRBdll(t, x(0), b)
        SecondVirial = ConvertUnitsRefProp("-V", Units, b, 0)
    End Function

    Function dPdrho(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM2dll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, z, hjt, aHelm, Gibbs, xkappa, beta, dPdD_T, d2PdD2_rho, dPdT_rho, dDdT_P, dDdP_rho, spare1, spare2, spare3, spare4)
        dPdrho = ConvertUnitsRefProp("-P", Units, 1 / ConvertUnitsRefProp("-D", Units, 1 / dPdD_T, 0), 0)
    End Function

    Function d2Pdrho2(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM2dll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, z, hjt, aHelm, Gibbs, xkappa, beta, dPdD_T, d2PdD2_rho, dPdT_rho, dDdT_P, dDdP_rho, spare1, spare2, spare3, spare4)
        d2Pdrho2 = ConvertUnitsRefProp("-P", Units, 1 / ConvertUnitsRefProp("-D", Units, ConvertUnitsRefProp("-D", Units, 1 / d2PdD2_rho, 0), 0), 0)
    End Function

    Function dPdT(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM2dll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, z, hjt, aHelm, Gibbs, xkappa, beta, dPdD_T, d2PdD2_rho, dPdT_rho, dDdT_P, dDdP_rho, spare1, spare2, spare3, spare4)
        dPdT = 0
        If dPdT_rho <> 0 Then dPdT = ConvertUnitsRefProp("-P", Units, 1 / ConvertUnitsRefProp("-A", Units, 1 / dPdT_rho, 0), 0)
    End Function

    Function drhodT(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        Call THERM2dll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, z, hjt, aHelm, Gibbs, xkappa, beta, dPdD_T, d2PdD2_rho, dPdT_rho, dDdT_P, dDdP_rho, spare1, spare2, spare3, spare4)
        drhodT = 0
        If dDdT_P <> 0 Then drhodT = ConvertUnitsRefProp("-D", Units, 1 / ConvertUnitsRefProp("-A", Units, 1 / dDdT_P, 0), 0)
    End Function

    Function Cstar(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim v As Double, cs As Double, ts As Double, Ds As Double, ps As Double, ws As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        v = 0
        On Error GoTo ErrorHandler
        Call CSTARdll(t, p, v, x(0), cs, ts, Ds, ps, ws, ierr, herr, 255)
        Return cs
ErrorHandler:
        Call CCRITdll(t, p, v, x(0), cs, ts, Ds, ps, ws, ierr, herr, 255)   'Old format
        Cstar = cs
    End Function


    Function Quality(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Dim xlkg(MaxComps) As Double, xvkg(MaxComps) As Double, qkg As Double, wliq As Double, wvap As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then Quality = Trim2(herr) : Exit Function
        Quality = q
        If q = 990 Then Quality = Trim2("Not calculated")
        If q = 998 Then Quality = Trim2("Superheated vapor with T>Tc")
        If q = 999 Then Quality = Trim2("Supercritical state (T>Tc, p>pc)")
        If q = -998 Then Quality = Trim2("Subcooled liquid with p>pc")
        If q > 0 And q < 1 And molmass = 2 Then
            Call QMASSdll(q, xliq(0), xvap(0), qkg, xlkg(0), xvkg(0), wliq, wvap, ierr, herr, 255)
            Quality = qkg
        End If
    End Function

    Function LiquidMoleFraction(ByRef FluidName As String, Optional ByRef InpCode As String = Nothing, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing, Optional ByVal i As Integer = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then LiquidMoleFraction = Trim2(herr) : Exit Function
        If i = Nothing Then LiquidMoleFraction = Trim2("Component number is missing") : Exit Function
        If i < 1 Or i > nc Then LiquidMoleFraction = Trim2("Constituent number out of range") : Exit Function
        If q < 0 Or q > 1 Then
            LiquidMoleFraction = x(i)
        Else
            LiquidMoleFraction = xliq(i)
        End If
        If nc = 1 Then LiquidMoleFraction = Trim2("Not applicable for a pure fluid")
    End Function

    Function VaporMoleFraction(ByRef FluidName As String, Optional ByRef InpCode As String = Nothing, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing, Optional ByVal i As Integer = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then VaporMoleFraction = Trim2(herr) : Exit Function
        If i = Nothing Then VaporMoleFraction = Trim2("Component number is missing") : Exit Function
        If i < 1 Or i > nc Then VaporMoleFraction = Trim2("Constituent number out of range") : Exit Function
        If q < 0 Or q > 1 Then
            VaporMoleFraction = x(i)
        Else
            VaporMoleFraction = xvap(i)
        End If

        If nc = 1 Then VaporMoleFraction = Trim2("Not applicable for a pure fluid")
    End Function

    Function Fugacity(ByRef FluidName As String, Optional ByRef InpCode As String = Nothing, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing, Optional ByVal i As Integer = Nothing)
        Dim f(MaxComps) As Double
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then Fugacity = Trim2(herr) : Exit Function
        If i = Nothing Then Fugacity = Trim2("Component number is missing ") : Exit Function
        If i < 1 Or i > nc Then Fugacity = Trim2("Constituent number out of Range ") : Exit Function
        Call FGCTYdll(t, d, x(0), f(0))
        Fugacity = ConvertUnitsRefProp("-P", Units, f(i), 0)
    End Function

    Function Viscosity(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then Viscosity = Trim2(herr) : Exit Function
        If t = 0 Or d = 0 Then Viscosity = Trim2("Inputs out of range") : Exit Function
        Call TRNPRPdll(t, d, x(0), eta, tcx, ierr2, herr2, 255)
        If q > 0.000001 And q < 1 - 0.000001 Then eta = -9999999
        Viscosity = ConvertUnitsRefProp("-U", Units, eta, 0)
        If eta = 0 Then Viscosity = Trim2("Unable to calculate property")
    End Function

    Function ThermalConductivity(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then ThermalConductivity = Trim2(herr) : Exit Function
        If t = 0 Or d = 0 Then ThermalConductivity = Trim2("Inputs out of range") : Exit Function
        Call TRNPRPdll(t, d, x(0), eta, tcx, ierr2, herr2, 255)
        If q > 0.000001 And q < 1 - 0.000001 Then tcx = -9999999
        ThermalConductivity = ConvertUnitsRefProp("-K", Units, tcx, 0)
        If tcx = 0 Then ThermalConductivity = Trim2("Unable to calculate property")
    End Function

    Function Prandtl(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then Prandtl = Trim2(herr) : Exit Function
        If t = 0 Or d = 0 Then Prandtl = Trim2("Inputs out of range") : Exit Function
        Call TRNPRPdll(t, d, x(0), eta, tcx, ierr2, herr2, 255)
        If q > 0.000001 And q < 1 - 0.000001 Then Prandtl = Trim2("Undefined") : Exit Function
        If tcx = 0 Or eta = 0 Then Prandtl = Trim2("Unable to calculate property")
        Call THERMdll(t, d, x(0), p, e, h, s, Cvcalc, Cpcalc, w, hjt)
        Call WMOLdll(x(0), wm)
        Prandtl = eta * Cpcalc / tcx / wm / 1000
    End Function

    Function SurfaceTension(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then SurfaceTension = Trim2(herr) : Exit Function
        If t = 0 Then SurfaceTension = Trim2("Input temperature is zero") : Exit Function
        If q >= 0 And q <= 1 Then
            Call SURFTdll(t, Dl, xliq(0), sigma, ierr2, herr2, 255)
        Else
            Call SURFTdll(t, d, x(0), sigma, ierr2, herr2, 255)
        End If
        SurfaceTension = ConvertUnitsRefProp("-N", Units, sigma, 0)
        If sigma = 0 Or ierr2 <> 0 Then SurfaceTension = Trim2("Inputs out of range")
    End Function

    Function DielectricConstant(ByRef FluidName As String, ByRef InpCode As String, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcProp(FluidName, InpCode, Units, Prop1, Prop2)
        If ierr > 0 Then DielectricConstant = Trim2(herr) : Exit Function
        If q > 0.000001 And q < 1 - 0.000001 Then DielectricConstant = Trim2("Undefined") : Exit Function
        If t = 0 Then DielectricConstant = Trim2("Inputs out of range") : Exit Function
        Call DIELECdll(t, d, x(0), de)
        DielectricConstant = de
    End Function

    Function MolarMass(ByRef FluidName As String, Optional ByRef InpCode As String = Nothing, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing) As Object
        Call CalcSetup(FluidName, "", Units, Prop1, Prop2)
        Call WMOLdll(x(0), wm)
        MolarMass = wm
    End Function

    Function MoleFraction(ByRef FluidName As String, ByVal i As Integer) As Object
        Call CalcProp(FluidName, "", "", 0, 0)
        If ierr > 0 Then MoleFraction = Trim2(herr) : Exit Function
        If i < 0 Or i > nc Then MoleFraction = Trim2("Constituent number out of range") : Exit Function
        MoleFraction = x(i - 1)
        If nc = 1 Then MoleFraction = Trim2("Not applicable for a pure fluid")
    End Function

    Function MassFraction(ByRef FluidName As String, ByVal i As Integer) As Object
        Call CalcProp(FluidName, "", "", 0, 0)
        If ierr > 0 Then MassFraction = Trim2(herr) : Exit Function
        If i < 1 Or i > nc Then MassFraction = Trim2("Constituent number out of range") : Exit Function
        Call XMASSdll(x(0), xmm(0), wm)
        MassFraction = xmm(i)
        If nc = 1 Then MassFraction = Trim2("Not applicable for a pure fluid")
    End Function

    'Change molar composition to mass composition
    'Prop1 - Prop20 are the molar values for the components in the mixture.
    'i specifies which component's mole fraction is returned.  If zero, the molar mass is returned
    Function Mole2Mass(ByRef FluidName As String, ByVal i As Integer, ByVal prop1 As Double, ByVal prop2 As Double, Optional ByVal prop3 As Double = Nothing, Optional ByVal prop4 As Double = Nothing, Optional ByVal prop5 As Double = Nothing, Optional ByVal prop6 As Double = Nothing, Optional ByVal prop7 As Double = Nothing, Optional ByVal prop8 As Double = Nothing, Optional ByVal prop9 As Double = Nothing, Optional ByVal prop10 As Double = Nothing, Optional ByVal prop11 As Double = Nothing, Optional ByVal prop12 As Double = Nothing, Optional ByVal prop13 As Double = Nothing, Optional ByVal prop14 As Double = Nothing, Optional ByVal prop15 As Double = Nothing, Optional ByVal prop16 As Double = Nothing, Optional ByVal prop17 As Double = Nothing, Optional ByVal prop18 As Double = Nothing, Optional ByVal prop19 As Double = Nothing, Optional ByVal prop20 As Double = Nothing) As Object
        Dim j As Integer, xkg2(MaxComps) As Double, xmol2(MaxComps) As Double, wmix2 As Double, sum As Double
        For j = 0 To MaxComps : xmol2(j) = 0 : Next
        xmol2(0) = prop1
        xmol2(1) = prop2
        If prop3 <> Nothing And IsNumeric(prop3) = True Then xmol2(2) = prop3
        If prop4 <> Nothing And IsNumeric(prop4) = True Then xmol2(3) = prop4
        If prop5 <> Nothing And IsNumeric(prop5) = True Then xmol2(4) = prop5
        If prop6 <> Nothing And IsNumeric(prop6) = True Then xmol2(5) = prop6
        If prop7 <> Nothing And IsNumeric(prop7) = True Then xmol2(6) = prop7
        If prop8 <> Nothing And IsNumeric(prop8) = True Then xmol2(7) = prop8
        If prop9 <> Nothing And IsNumeric(prop9) = True Then xmol2(8) = prop9
        If prop10 <> Nothing And IsNumeric(prop10) = True Then xmol2(9) = prop10
        If prop11 <> Nothing And IsNumeric(prop11) = True Then xmol2(10) = prop11
        If prop12 <> Nothing And IsNumeric(prop12) = True Then xmol2(11) = prop12
        If prop13 <> Nothing And IsNumeric(prop13) = True Then xmol2(12) = prop13
        If prop14 <> Nothing And IsNumeric(prop14) = True Then xmol2(13) = prop14
        If prop15 <> Nothing And IsNumeric(prop15) = True Then xmol2(14) = prop15
        If prop16 <> Nothing And IsNumeric(prop16) = True Then xmol2(15) = prop16
        If prop17 <> Nothing And IsNumeric(prop17) = True Then xmol2(16) = prop17
        If prop18 <> Nothing And IsNumeric(prop18) = True Then xmol2(17) = prop18
        If prop19 <> Nothing And IsNumeric(prop19) = True Then xmol2(18) = prop19
        If prop20 <> Nothing And IsNumeric(prop20) = True Then xmol2(19) = prop20
        Call CalcSetup(FluidName, "", "", 0, 0)
        If ierr > 0 Then Mole2Mass = Trim2(herr) : Exit Function
        If i < 0 Or i > nc Then Mole2Mass = Trim2("Index out of Range (greater than number of components in mixture)") : Exit Function
        sum = 0
        For j = 0 To nc
            sum = sum + xmol2(j)
        Next
        If Math.Abs(sum - 1) > 0.0001 Then Mole2Mass = Trim2("Composition does not sum to 1") : Exit Function
        Call XMASSdll(xmol2(0), xkg2(0), wmix2)
        If i = 0 Then  'Molar mass of mixture
            Mole2Mass = wmix2
        Else               'Mass fraction
            Mole2Mass = xkg2(i - 1)
        End If
    End Function

    'Change mass composition to molar composition
    'Prop1 - Prop20 are the mass values for the components in the mixture.
    'i specifies which component's mass fraction is returned.  If zero, the molar mass is returned
    Function Mass2Mole(ByRef FluidName As String, ByVal i As Integer, ByVal prop1 As Double, ByVal prop2 As Double, Optional ByVal prop3 As Double = Nothing, Optional ByVal prop4 As Double = Nothing, Optional ByVal prop5 As Double = Nothing, Optional ByVal prop6 As Double = Nothing, Optional ByVal prop7 As Double = Nothing, Optional ByVal prop8 As Double = Nothing, Optional ByVal prop9 As Double = Nothing, Optional ByVal prop10 As Double = Nothing, Optional ByVal prop11 As Double = Nothing, Optional ByVal prop12 As Double = Nothing, Optional ByVal prop13 As Double = Nothing, Optional ByVal prop14 As Double = Nothing, Optional ByVal prop15 As Double = Nothing, Optional ByVal prop16 As Double = Nothing, Optional ByVal prop17 As Double = Nothing, Optional ByVal prop18 As Double = Nothing, Optional ByVal prop19 As Double = Nothing, Optional ByVal prop20 As Double = Nothing) As Object
        Dim j As Integer, xkg2(MaxComps) As Double, xmol2(MaxComps) As Double, wmix2 As Double, sum As Double
        For j = 0 To MaxComps : xkg2(j) = 0 : Next
        xkg2(0) = prop1
        xkg2(1) = prop2
        If prop3 <> Nothing And IsNumeric(prop3) = True Then xkg2(2) = prop3
        If prop4 <> Nothing And IsNumeric(prop4) = True Then xkg2(3) = prop4
        If prop5 <> Nothing And IsNumeric(prop5) = True Then xkg2(4) = prop5
        If prop6 <> Nothing And IsNumeric(prop6) = True Then xkg2(5) = prop6
        If prop7 <> Nothing And IsNumeric(prop7) = True Then xkg2(6) = prop7
        If prop8 <> Nothing And IsNumeric(prop8) = True Then xkg2(7) = prop8
        If prop9 <> Nothing And IsNumeric(prop9) = True Then xkg2(8) = prop9
        If prop10 <> Nothing And IsNumeric(prop10) = True Then xkg2(9) = prop10
        If prop11 <> Nothing And IsNumeric(prop11) = True Then xkg2(10) = prop11
        If prop12 <> Nothing And IsNumeric(prop12) = True Then xkg2(11) = prop12
        If prop13 <> Nothing And IsNumeric(prop13) = True Then xkg2(12) = prop13
        If prop14 <> Nothing And IsNumeric(prop14) = True Then xkg2(13) = prop14
        If prop15 <> Nothing And IsNumeric(prop15) = True Then xkg2(14) = prop15
        If prop16 <> Nothing And IsNumeric(prop16) = True Then xkg2(15) = prop16
        If prop17 <> Nothing And IsNumeric(prop17) = True Then xkg2(16) = prop17
        If prop18 <> Nothing And IsNumeric(prop18) = True Then xkg2(17) = prop18
        If prop19 <> Nothing And IsNumeric(prop19) = True Then xkg2(18) = prop19
        If prop20 <> Nothing And IsNumeric(prop20) = True Then xkg2(19) = prop20
        Call CalcSetup(FluidName, "", "", 0, 0)
        If ierr > 0 Then Mass2Mole = Trim2(herr) : Exit Function
        If i < 0 Or i > nc Then Mass2Mole = Trim2("Index out of Range (greater than number of components in mixture)") : Exit Function
        sum = 0
        For j = 0 To nc
            sum = sum + xkg2(j)
        Next
        If Math.Abs(sum - 1) > 0.0001 Then Mass2Mole = Trim2("Composition does not sum to 1") : Exit Function
        Call XMOLEdll(xkg2(0), xmol2(0), wmix2)
        If i = 0 Then  'Molar mass of mixture
            Mass2Mole = wmix2
        Else               'Mole fraction
            Mass2Mole = xmol2(i - 1)
        End If
    End Function

    Function EOSMax(ByRef FluidName As String, Optional ByRef InpCode As String = Nothing, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcSetup(FluidName, "", Units, Prop1, Prop2)
        If nc > 1 Then
            Call LIMITXdll("EOS", 300.0, 0.0, 0.0, x(0), tmin, tmax, dmax, pmax, ierr2, herr2, 3, 255)
        Else
            Call LIMITKdll("EOS", 1, 300.0, 0.0, 0.0, tmin, tmax, dmax, pmax, ierr2, herr2, 3, 255)
        End If
        If InpCode = Nothing Then InpCode = ""
        If InpCode = "P" Or InpCode = "p" Then
            EOSMax = ConvertUnitsRefProp("-P", Units, pmax, 0)
        ElseIf InpCode = "D" Or InpCode = "d" Then
            EOSMax = ConvertUnitsRefProp("-D", Units, dmax, 0)
        Else
            EOSMax = ConvertUnitsRefProp("-T", Units, tmax, 0)
        End If
    End Function

    Function EOSMin(ByRef FluidName As String, Optional ByRef InpCode As String = Nothing, Optional ByRef Units As String = Nothing, Optional ByVal Prop1 As Double = Nothing, Optional ByVal Prop2 As Double = Nothing)
        Call CalcSetup(FluidName, "", Units, Prop1, Prop2)
        If nc > 1 Then
            Call LIMITXdll("EOS", 300.0#, 0.0#, 0.0#, x(0), tmin, tmax, dmax, pmax, ierr2, herr2, 3, 255)
        Else
            Call LIMITKdll("EOS", 1, 300.0#, 0.0#, 0.0#, tmin, tmax, dmax, pmax, ierr2, herr2, 3, 255)
        End If
        If InpCode = Nothing Then InpCode = ""
        If InpCode = "P" Or InpCode = "p" Then
            EOSMin = 0
        ElseIf InpCode = "D" Or InpCode = "d" Then
            EOSMin = 0
        Else
            EOSMin = ConvertUnitsRefProp("-T", Units, tmin, 0)
        End If
    End Function

    Function ErrorCode(ByVal InputCell)
        ErrorCode = ierr
    End Function

    Function ErrorString(ByVal InputCell)
        ErrorString = Trim2(herr)
    End Function

    Function Trim2(ByVal a)
        'All error messages call this routine to add the pound sign (#) to the beginning of the line.
        'If you do not want this error code, simply remove the ["#" +] piece below.
        'It can also be changed to any other symbol(s) you desire.
        If Left(a, 1) <> "#" Then
            Trim2 = "#" + Trim(a)
        Else
            Trim2 = Trim(a)
        End If
    End Function

    Function UnitConvert(ByVal InputValue As Object, ByVal UnitType As String, ByVal OldUnits As String, ByVal NewUnits As String)

        'InputValue is the value to be converted from OldUnits to NewUnits
        'UnitType is one of the following letters (one character only in most cases):
        'UnitType     Unit name                          SI units
        '  T         Temperature                            K
        '  P         Pressure                               Pa
        '  D         Density or specific volume         mol/m^3 or kg/m^3 (or m^3/mol or m^3/kg)
        '  H         Enthalpy or specific energy        J/mol or J/kg
        '  S         Entropy or heat capacity           J/mol-K or J/kg-K
        '  W         Speed of sound                         m/s
        '  U         Viscosity                              Pa-s
        '  K         Thermal conductivity                   W/m-K
        '  JT        Joule Thomson                          K/Pa
        '  L         Length                                 m
        '  A         Area                                   m^2
        '  V         Volume                                 m^3
        '  M         Mass                                   kg
        '  F         Force                                  N
        '  E         Energy                                 J
        '  Q         Power                                  W
        '  N         Surface tension                        N/m
        ' Gage pressures can be used by adding "_g" to the unit, e.g., "MPa_g"

        Dim Value As Double, Tpe As String, Unit1 As String, Unit2 As String
        Dim Drct As Integer, Gage As Integer, Vacm As Integer
        Dim MolWt As Double, Rgas As Double

        If Not IsNumeric(InputValue) Then UnitConvert = 0 : Exit Function
        If NewUnits = "" Then UnitConvert = InputValue : Exit Function
        Value = InputValue
        Tpe = UCase(Trim(UnitType))
        Unit1 = UCase(Trim(OldUnits))
        Unit2 = UCase(Trim(NewUnits))

        Rgas = 8.314472
        Call WMOLdll(x(0), wm)
        If CompFlag = 1 Then Call WMOLdll(xliq(0), wm)
        If CompFlag = 2 Then Call WMOLdll(xvap(0), wm)
        MolWt = wm

        For Drct = 1 To -1 Step -2
            '-----------------------------------------------------------------------
            '   Temperature Conversion
            '-----------------------------------------------------------------------
            If Tpe = "T" Then
                If Unit1 = "K" Then
                ElseIf Unit1 = "C" Then
                    Value = Value + Drct * CtoK
                ElseIf Unit1 = "R" Then
                    Value = Value * RtoK ^ Drct
                ElseIf Unit1 = "F" Then
                    If Drct = 1 Then
                        'Value = RtoK * (Value + FtoR)    'Does not give exactly zero at 32 F
                        Value = (Value - 32) * RtoK + CtoK
                    Else
                        'Value = Value / RtoK - FtoR      'Does not give exactly 32 at 273.15 K
                        Value = (Value - CtoK) / RtoK + 32
                    End If
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Pressure Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "P" Then
                Gage = InStr(Unit1, "GAGE")
                Vacm = InStr(Unit1, "VACM")
                If Gage = 0 Then Gage = InStr(Unit1, "_G")
                If Vacm = 0 Then Vacm = InStr(Unit1, "_V")
                If Gage <> 0 And Drct = -1 Then Value = Value - ATMtoMPa
                If Vacm <> 0 And Drct = -1 Then Value = ATMtoMPa - Value
                If Gage <> 0 Then Unit1 = Trim(Left(Unit1, Gage - 1))
                If Vacm <> 0 Then Unit1 = Trim(Left(Unit1, Vacm - 1))
                If Unit1 = "PA" Then
                    Value = Value / 1000000 ^ Drct
                ElseIf Unit1 = "KPA" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "MPA" Then
                    Value = Value
                ElseIf Unit1 = "GPA" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "BAR" Then
                    Value = Value * BARtoMPA ^ Drct
                ElseIf Unit1 = "KBAR" Then
                    Value = Value * (BARtoMPA * 1000) ^ Drct
                ElseIf Unit1 = "ATM" Then
                    Value = Value * ATMtoMPa ^ Drct
                ElseIf Unit1 = "KGF/CM^2" Or Unit1 = "KG/CM^2" Or Unit1 = "ATA" Or Unit1 = "AT" Or Unit1 = "ATMA" Then
                    Value = Value * (KGFtoN / 100) ^ Drct
                ElseIf Unit1 = "PSI" Or Unit1 = "PSIA" Then
                    Value = Value * PSIAtoMPA ^ Drct
                ElseIf Unit1 = "PSF" Then
                    Value = Value * (PSIAtoMPA / 144) ^ Drct
                ElseIf Unit1 = "MMHG" Or Unit1 = "TORR" Then
                    Value = Value * MMHGtoMPA ^ Drct
                ElseIf Unit1 = "CMHG" Then
                    Value = Value * (MMHGtoMPA * 10) ^ Drct
                ElseIf Unit1 = "INHG" Then
                    Value = Value * (MMHGtoMPA * INtoM * 1000) ^ Drct
                ElseIf Unit1 = "INH2O" Then
                    Value = Value * INH2OtoMPA ^ Drct
                ElseIf Unit1 = "PSIG" Then
                    If Drct = 1 Then
                        Value = PSIAtoMPA * Value + ATMtoMPa
                    Else
                        Value = (Value - ATMtoMPa) / PSIAtoMPA
                    End If
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If
                If Gage <> 0 And Drct = 1 Then Value = Value + ATMtoMPa
                If Vacm <> 0 And Drct = 1 Then Value = ATMtoMPa - Value

                '-----------------------------------------------------------------------
                '   Density Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "D" Then
                If Value = 0 Then Value = 1.0E-50
                If Unit1 = "MOL/DM^3" Or Unit1 = "MOL/L" Or Unit1 = "KMOL/M^3" Then
                ElseIf Unit1 = "MOL/CM^3" Or Unit1 = "MOL/CC" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "MOL/M^3" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "KG/M^3" Then
                    Value = Value / MolWt ^ Drct
                ElseIf Unit1 = "KG/DM^3" Or Unit1 = "KG/L" Then
                    Value = Value * (1000 / MolWt) ^ Drct
                ElseIf Unit1 = "G/DM^3" Or Unit1 = "G/L" Then
                    Value = Value * (1 / MolWt) ^ Drct
                ElseIf Unit1 = "G/CC" Or Unit1 = "G/CM^3" Or Unit1 = "G/ML" Then
                    Value = Value * (1000 / MolWt) ^ Drct
                ElseIf Unit1 = "G/DM^3" Then
                    Value = Value * (1 / MolWt) ^ Drct
                ElseIf Unit1 = "LBM/FT^3" Or Unit1 = "LB/FT^3" Then
                    Value = Value * (LBMtoKG / FT3toM3 / MolWt) ^ Drct
                ElseIf Unit1 = "LBMOL/FT^3" Then
                    Value = Value * (LBMtoKG / FT3toM3) ^ Drct
                ElseIf Unit1 = "SLUG/FT^3" Then
                    Value = Value * (LBMtoKG / FT3toM3 / MolWt * KGFtoN / FTtoM) ^ Drct
                ElseIf Unit1 = "LBMOL/GAL" Then
                    Value = Value * (LBMtoKG / GALLONtoM3) ^ Drct
                ElseIf Unit1 = "LB/GAL" Or Unit1 = "LBM/GAL" Then
                    Value = Value * (LBMtoKG / GALLONtoM3 / MolWt) ^ Drct

                    '-----------------------------------------------------------------------
                    '   Specific Volume Conversion
                    '-----------------------------------------------------------------------
                ElseIf Unit1 = "DM^3/MOL" Or Unit1 = "L/MOL" Or Unit1 = "M^3/KMOL" Then
                    Value = 1 / Value
                ElseIf Unit1 = "CM^3/MOL" Or Unit1 = "CC/MOL" Or Unit1 = "ML/MOL" Then
                    Value = 1000 / Value
                ElseIf Unit1 = "M^3/MOL" Then
                    Value = 1 / Value / 1000
                ElseIf Unit1 = "M^3/KG" Then
                    Value = 1 / Value / MolWt
                ElseIf Unit1 = "DM^3/KG" Or Unit1 = "L/KG" Then
                    Value = 1000 / Value / MolWt
                ElseIf Unit1 = "CC/G" Or Unit1 = "CM^3/G" Or Unit1 = "ML/G" Then
                    Value = 1000 / Value / MolWt
                ElseIf Unit1 = "DM^3/G" Then
                    Value = 1 / Value / MolWt
                ElseIf Unit1 = "FT^3/LBM" Or Unit1 = "FT^3/LB" Then
                    Value = 1 / Value * (LBMtoKG / FT3toM3 / MolWt)
                ElseIf Unit1 = "FT^3/LBMOL" Then
                    Value = 1 / Value * (LBMtoKG / FT3toM3)
                ElseIf Unit1 = "FT^3/SLUG" Then
                    Value = 1 / Value * (LBMtoKG / FT3toM3 / MolWt * KGFtoN / FTtoM)
                ElseIf Unit1 = "GAL/LBMOL" Then
                    Value = 1 / Value * (LBMtoKG / GALLONtoM3)
                ElseIf Unit1 = "GAL/LB" Or Unit1 = "GAL/LBM" Then
                    Value = 1 / Value * (LBMtoKG / GALLONtoM3 / MolWt)
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If
                If Math.Abs(Value) < 1.0E-30 Then Value = 0

                '-----------------------------------------------------------------------
                '   Specific Energy and Enthalpy Conversions
                '-----------------------------------------------------------------------
            ElseIf Tpe = "H" Then
                If Unit1 = "J/MOL" Or Unit1 = "KJ/KMOL" Then
                ElseIf Unit1 = "KJ/MOL" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "MJ/MOL" Then
                    Value = Value * 1000000 ^ Drct
                ElseIf Unit1 = "KJ/KG" Or Unit1 = "J/G" Then
                    Value = MolWt ^ Drct * Value
                ElseIf Unit1 = "J/KG" Then
                    Value = (MolWt / 1000) ^ Drct * Value
                ElseIf Unit1 = "M^2/S^2" Then
                    Value = (MolWt / 1000) ^ Drct * Value
                ElseIf Unit1 = "FT^2/S^2" Then
                    Value = (MolWt / 1000 * FTtoM ^ 2) ^ Drct * Value
                ElseIf Unit1 = "CAL/MOL" Or Unit1 = "KCAL/KMOL" Then
                    Value = CALtoJ ^ Drct * Value
                ElseIf Unit1 = "CAL/G" Or Unit1 = "KCAL/KG" Then
                    Value = (CALtoJ * MolWt) ^ Drct * Value
                ElseIf Unit1 = "BTU/LBM" Or Unit1 = "BTU/LB" Then
                    Value = (BTUtoKJ / LBMtoKG * MolWt) ^ Drct * Value
                ElseIf Unit1 = "BTU/LBMOL" Then
                    Value = (BTUtoKJ / LBMtoKG) ^ Drct * Value
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Entropy and Heat Capacity Conversions
                '-----------------------------------------------------------------------
            ElseIf Tpe = "S" Then
                If Unit1 = "J/MOL-K" Or Unit1 = "KJ/KMOL-K" Then
                    Value = Value
                ElseIf Unit1 = "KJ/MOL-K" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "KJ/KG-K" Or Unit1 = "J/G-K" Then
                    Value = MolWt ^ Drct * Value
                ElseIf Unit1 = "J/KG-K" Then
                    Value = (MolWt / 1000) ^ Drct * Value
                ElseIf Unit1 = "BTU/LBM-R" Or Unit1 = "BTU/LB-R" Then
                    Value = (BTUtoKJ / LBMtoKG / RtoK * MolWt) ^ Drct * Value
                ElseIf Unit1 = "BTU/LBMOL-R" Then
                    Value = (BTUtoKJ / LBMtoKG / RtoK) ^ Drct * Value
                ElseIf Unit1 = "CAL/G-K" Or Unit1 = "CAL/G-C" Or Unit1 = "KCAL/KG-K" Or Unit1 = "KCAL/KG-C" Then
                    Value = (CALtoJ * MolWt) ^ Drct * Value
                ElseIf Unit1 = "CAL/MOL-K" Or Unit1 = "CAL/MOL-C" Then
                    Value = CALtoJ ^ Drct * Value
                ElseIf Unit1 = "FT-LBF/LBMOL-R" Then
                    Value = (FTLBFtoJ / LBMtoKG / RtoK / 1000) ^ Drct * Value
                ElseIf Unit1 = "CP/R" Then
                    Value = Rgas ^ Drct * Value * 1000
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Speed of Sound Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "W" Then
                If Unit1 = "M/S" Then
                ElseIf Unit1 = "M^2/S^2" Then
                    Value = Math.Sqrt(Value)
                ElseIf Unit1 = "CM/S" Then
                    Value = Value / 100 ^ Drct
                ElseIf Unit1 = "KM/H" Then
                    Value = Value * (1000 / HtoS) ^ Drct
                ElseIf Unit1 = "FT/S" Then
                    Value = Value * FTtoM ^ Drct
                ElseIf Unit1 = "IN/S" Then
                    Value = Value * INtoM ^ Drct
                ElseIf Unit1 = "MILE/H" Or Unit1 = "MPH" Then
                    Value = Value * (INtoM * 63360 / HtoS) ^ Drct
                ElseIf Unit1 = "KNOT" Then
                    Value = Value * 0.5144444444 ^ Drct
                ElseIf Unit1 = "MACH" Then
                    Value = Value * Math.Sqrt(1.4 * 298.15 * 8314.51 / 28.95853816) ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Viscosity Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "U" Then
                If Unit1 = "PA-S" Or Unit1 = "KG/M-S" Then
                ElseIf Unit1 = "MPA-S" Then      'Note:  This is milliPa-s, not MPa-s
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "UPA-S" Then
                    Value = Value / 1000000 ^ Drct
                ElseIf Unit1 = "G/CM-S" Or Unit1 = "POISE" Then
                    Value = Value / 10 ^ Drct
                ElseIf Unit1 = "CENTIPOISE" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "MILLIPOISE" Or Unit1 = "MPOISE" Then
                    Value = Value / 10000 ^ Drct
                ElseIf Unit1 = "MICROPOISE" Or Unit1 = "UPOISE" Then
                    Value = Value / 10000000 ^ Drct
                ElseIf Unit1 = "LBM/FT-S" Or Unit1 = "LB/FT-S" Then
                    Value = Value * (LBMtoKG / FTtoM) ^ Drct
                ElseIf Unit1 = "LBF-S/FT^2" Then
                    Value = Value * (LBFtoN / FTtoM ^ 2) ^ Drct
                ElseIf Unit1 = "LBM/FT-H" Or Unit1 = "LB/FT-H" Then
                    Value = Value * (LBMtoKG / FTtoM / HtoS) ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Thermal Conductivity Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "K" Then
                If Unit1 = "MW/M-K" Then
                ElseIf Unit1 = "W/M-K" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "G-CM/S^3-K" Then
                    Value = Value / 100 ^ Drct
                ElseIf Unit1 = "KG-M/S^3-K" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "CAL/S-CM-K" Then
                    Value = Value * (CALtoJ * 100000) ^ Drct
                ElseIf Unit1 = "KCAL/HR-M-K" Then
                    Value = Value * (CALtoJ * 100000 * 1000 / 100 / 3600) ^ Drct
                ElseIf Unit1 = "LBM-FT/S^3-F" Or Unit1 = "LB-FT/S^3-F" Then
                    Value = Value * (1000 * LBMtoKG * FTtoM / RtoK) ^ Drct
                ElseIf Unit1 = "LBF/S-F" Then
                    Value = Value * (1000 * LBFtoN / RtoK) ^ Drct
                ElseIf Unit1 = "BTU/H-FT-F" Then
                    Value = Value * (1000 * BTUtoW / HtoS / FTtoM / RtoK) ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Joule-Thomson Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "JT" Then
                If Unit1 = "K/MPA" Or Unit1 = "C/MPA" Then
                ElseIf Unit1 = "K/KPA" Or Unit1 = "C/KPA" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "K/PA" Or Unit1 = "C/PA" Then
                    Value = Value * 1000000 ^ Drct
                ElseIf Unit1 = "C/ATM" Then
                    Value = Value / ATMtoMPa ^ Drct
                ElseIf Unit1 = "C/BAR" Then
                    Value = Value / BARtoMPA ^ Drct
                ElseIf Unit1 = "K/PSI" Or Unit1 = "K/PSIA" Then
                    Value = Value / PSIAtoMPA ^ Drct
                ElseIf Unit1 = "F/PSI" Or Unit1 = "F/PSIA" Or Unit1 = "R/PSIA" Then
                    Value = Value / (PSIAtoMPA / RtoK) ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Length Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "L" Then
                If Unit1 = "METER" Or Unit1 = "M" Then
                ElseIf Unit1 = "DM" Then
                    Value = Value / 10 ^ Drct
                ElseIf Unit1 = "CM" Then
                    Value = Value / 100 ^ Drct
                ElseIf Unit1 = "MM" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "KM" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "INCH" Or Unit1 = "IN" Then
                    Value = Value * INtoM ^ Drct
                ElseIf Unit1 = "FOOT" Or Unit1 = "FT" Then
                    Value = Value * FTtoM ^ Drct
                ElseIf Unit1 = "YARD" Or Unit1 = "YD" Then
                    Value = Value * (INtoM * 36) ^ Drct
                ElseIf Unit1 = "MILE" Or Unit1 = "MI" Then
                    Value = Value * (INtoM * 63360) ^ Drct
                ElseIf Unit1 = "LIGHT YEAR" Then
                    Value = Value * 9.46055E+15 ^ Drct
                ElseIf Unit1 = "ANGSTROM" Then
                    Value = Value / 10000000000.0# ^ Drct
                ElseIf Unit1 = "FATHOM" Then
                    Value = Value * (FTtoM * 6) ^ Drct
                ElseIf Unit1 = "MIL" Then
                    Value = Value * (INtoM / 1000) ^ Drct
                ElseIf Unit1 = "ROD" Then
                    Value = Value * (INtoM * 16.5 * 12) ^ Drct
                ElseIf Unit1 = "PARSEC" Then
                    Value = Value * (30837400000000.0# * 1000) ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Area Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "A" Then
                If Unit1 = "METER^2" Or Unit1 = "M^2" Then
                ElseIf Unit1 = "CM^2" Then
                    Value = Value / 10000 ^ Drct
                ElseIf Unit1 = "MM^2" Then
                    Value = Value / 1000000 ^ Drct
                ElseIf Unit1 = "KM^2" Then
                    Value = Value * 1000000 ^ Drct
                ElseIf Unit1 = "INCH^2" Or Unit1 = "IN^2" Then
                    Value = Value * (INtoM ^ 2) ^ Drct
                ElseIf Unit1 = "FOOT^2" Or Unit1 = "FT^2" Then
                    Value = Value * (FTtoM ^ 2) ^ Drct
                ElseIf Unit1 = "YARD^2" Or Unit1 = "YD^2" Then
                    Value = Value * ((INtoM * 36) ^ 2) ^ Drct
                ElseIf Unit1 = "MILE^2" Or Unit1 = "MI^2" Then
                    Value = Value * ((INtoM * 63360) ^ 2) ^ Drct
                ElseIf Unit1 = "ACRE" Then
                    Value = Value * ((INtoM * 36) ^ 2 * 4840) ^ Drct
                ElseIf Unit1 = "BARN" Then
                    Value = Value * 1.0E-28 ^ Drct
                ElseIf Unit1 = "HECTARE" Then
                    Value = Value * 10000 ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Volume Conversion (Note: not specific volume)
                '-----------------------------------------------------------------------
            ElseIf Tpe = "V" Then
                If Unit1 = "METER^3" Or Unit1 = "M^3" Then
                ElseIf Unit1 = "CM^3" Then
                    Value = Value / 1000000 ^ Drct
                ElseIf Unit1 = "LITER" Or Unit1 = "L" Or Unit1 = "DM^3" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "INCH^3" Or Unit1 = "IN^3" Then
                    Value = Value * IN3toM3 ^ Drct
                ElseIf Unit1 = "FOOT^3" Or Unit1 = "FT^3" Then
                    Value = Value * (IN3toM3 * 12 ^ 3) ^ Drct
                ElseIf Unit1 = "YARD^3" Or Unit1 = "YD^3" Then
                    Value = Value * (IN3toM3 * 36 ^ 3) ^ Drct
                ElseIf Unit1 = "GALLON" Or Unit1 = "GAL" Then
                    Value = Value * GALLONtoM3 ^ Drct
                ElseIf Unit1 = "QUART" Or Unit1 = "QT" Then
                    Value = Value * (GALLONtoM3 / 4) ^ Drct
                ElseIf Unit1 = "PINT" Or Unit1 = "PT" Then
                    Value = Value * (GALLONtoM3 / 8) ^ Drct
                ElseIf Unit1 = "CUP" Then
                    Value = Value * (GALLONtoM3 / 16) ^ Drct
                ElseIf Unit1 = "OUNCE" Then
                    Value = Value * (GALLONtoM3 / 128) ^ Drct
                ElseIf Unit1 = "TABLESPOON" Or Unit1 = "TBSP" Then
                    Value = Value * (GALLONtoM3 / 256) ^ Drct
                ElseIf Unit1 = "TEASPOON" Or Unit1 = "TSP" Then
                    Value = Value * (GALLONtoM3 / 768) ^ Drct
                ElseIf Unit1 = "CORD" Then
                    Value = Value * (FT3toM3 * 128) ^ Drct
                ElseIf Unit1 = "BARREL" Then
                    Value = Value * (GALLONtoM3 * 42) ^ Drct
                ElseIf Unit1 = "BOARD FOOT" Then
                    Value = Value * (IN3toM3 * 144) ^ Drct
                ElseIf Unit1 = "BUSHEL" Then
                    Value = Value * 0.03523907016688 ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Mass Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "M" Then
                If Unit1 = "KG" Then
                ElseIf Unit1 = "G" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "MG" Then            'milligram
                    Value = Value / 1000000 ^ Drct
                ElseIf Unit1 = "LBM" Or Unit1 = "LB" Then
                    Value = Value * LBMtoKG ^ Drct
                ElseIf Unit1 = "GRAIN" Then
                    Value = Value * (LBMtoKG / 7000) ^ Drct
                ElseIf Unit1 = "SLUG" Then
                    Value = Value * (KGFtoN * LBMtoKG / FTtoM) ^ Drct
                ElseIf Unit1 = "TON" Then
                    Value = Value * (LBMtoKG * 2000) ^ Drct
                ElseIf Unit1 = "TONNE" Then
                    Value = Value * 1000 ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Force Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "F" Then
                If Unit1 = "NEWTON" Or Unit1 = "N" Then
                ElseIf Unit1 = "MN" Then 'milliNewtons
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "KGF" Then
                    Value = Value * KGFtoN ^ Drct
                ElseIf Unit1 = "DYNE" Then
                    Value = Value / 100000 ^ Drct
                ElseIf Unit1 = "LBF" Then
                    Value = Value * LBFtoN ^ Drct
                ElseIf Unit1 = "POUNDAL" Then
                    Value = Value * (LBMtoKG * FTtoM) ^ Drct
                ElseIf Unit1 = "OZF" Then
                    Value = Value * (LBFtoN / 16) ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Energy Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "E" Then
                If Unit1 = "JOULE" Or Unit1 = "J" Then
                ElseIf Unit1 = "KJ" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "MJ" Then
                    Value = Value * 1000000 ^ Drct
                ElseIf Unit1 = "KW-H" Then
                    Value = Value * (HtoS * 1000) ^ Drct
                ElseIf Unit1 = "CAL" Then
                    Value = CALtoJ ^ Drct * Value
                ElseIf Unit1 = "KCAL" Then
                    Value = Value * (CALtoJ * 1000) ^ Drct
                ElseIf Unit1 = "ERG" Then
                    Value = Value / 10000000 ^ Drct
                ElseIf Unit1 = "BTU" Then
                    Value = Value * (BTUtoKJ * 1000) ^ Drct
                ElseIf Unit1 = "FT-LBF" Then
                    Value = Value * FTLBFtoJ ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Power Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "Q" Then
                If Unit1 = "WATT" Or Unit1 = "W" Then
                ElseIf Unit1 = "KWATT" Or Unit1 = "KW" Then
                    Value = Value * 1000 ^ Drct
                ElseIf Unit1 = "BTU/S" Then
                    Value = Value * BTUtoW ^ Drct
                ElseIf Unit1 = "BTU/MIN" Then
                    Value = Value * (BTUtoW / 60) ^ Drct
                ElseIf Unit1 = "BTU/H" Then
                    Value = Value * (BTUtoW / HtoS) ^ Drct
                ElseIf Unit1 = "CAL/S" Then
                    Value = Value * CALtoJ ^ Drct
                ElseIf Unit1 = "KCAL/S" Then
                    Value = Value * (CALtoJ * 1000) ^ Drct
                ElseIf Unit1 = "CAL/MIN" Then
                    Value = Value * (CALtoJ / 60) ^ Drct
                ElseIf Unit1 = "KCAL/MIN" Then
                    Value = Value * (CALtoJ / 60 * 1000) ^ Drct
                ElseIf Unit1 = "FT-LBF/S" Then
                    Value = Value * FTLBFtoJ ^ Drct
                ElseIf Unit1 = "FT-LBF/MIN" Then
                    Value = Value * (FTLBFtoJ / 60) ^ Drct
                ElseIf Unit1 = "FT-LBF/H" Then
                    Value = Value * (FTLBFtoJ / HtoS) ^ Drct
                ElseIf Unit1 = "HP" Then
                    Value = Value * HPtoW ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If

                '-----------------------------------------------------------------------
                '   Surface Tension Conversion
                '-----------------------------------------------------------------------
            ElseIf Tpe = "N" Then
                If Unit1 = "N/M" Then
                ElseIf Unit1 = "MN/M" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "DYNE/CM" Or Unit1 = "DYN/CM" Then
                    Value = Value / 1000 ^ Drct
                ElseIf Unit1 = "LBF/FT" Then
                    Value = Value * LBFTtoNM ^ Drct
                Else
                    UnitConvert = Trim2("Undefined input unit") : Exit Function
                End If
            End If
            Unit1 = Unit2
        Next Drct
        UnitConvert = Value
    End Function

    Sub SetupUnits(ByVal i As Integer)

        'Warning:  If any of these are changed (to make them the default) after the program has run,
        '  you will need to exit Excel and restart it so that it reinitializes

        'Refprop Units
        tUnits2 = "K"
        taUnits2 = "K"
        pUnits2 = "kPa"
        dUnits2 = "mol/dm^3"
        vUnits2 = "dm^3/mol"
        hUnits2 = "J/mol"
        sUnits2 = "J/mol-K"
        wUnits2 = "m/s"
        visUnits2 = "uPa-s"
        tcxUnits2 = "W/m-K"
        stUnits2 = "N/m"
        'Default units: (SI)
        tUnits(0) = "K"
        taUnits(0) = "K"
        pUnits(0) = "MPa"
        dUnits(0) = "kg/m^3"
        vUnits(0) = "m^3/kg"
        hUnits(0) = "kJ/kg"
        sUnits(0) = "kJ/kg-K"
        wUnits(0) = "m/s"
        visUnits(0) = "uPa-s"
        tcxUnits(0) = "mW/m-K"
        stUnits(0) = "mN/m"
        'Default units but with K switch to C (SI with C)
        tUnits(5) = "C"
        taUnits(5) = "K"
        pUnits(5) = "MPa"
        dUnits(5) = "kg/m^3"
        vUnits(5) = "m^3/kg"
        hUnits(5) = "kJ/kg"
        sUnits(5) = "kJ/kg-K"
        wUnits(5) = "m/s"
        visUnits(5) = "uPa-s"
        tcxUnits(5) = "mW/m-K"
        stUnits(5) = "mN/m"
        'Default units on a molar basis (Molar SI)
        tUnits(6) = "K"
        taUnits(6) = "K"
        pUnits(6) = "MPa"
        dUnits(6) = "mol/dm^3"
        vUnits(6) = "dm^3/mol"
        hUnits(6) = "J/mol"
        sUnits(6) = "J/mol-K"
        wUnits(6) = "m/s"
        visUnits(6) = "uPa-s"
        tcxUnits(6) = "mW/m-K"
        stUnits(6) = "mN/m"
        'mks (mks)
        tUnits(1) = "K"
        taUnits(1) = "K"
        pUnits(1) = "kPa"
        dUnits(1) = "kg/m^3"
        vUnits(1) = "m^3/kg"
        hUnits(1) = "kJ/kg"
        sUnits(1) = "kJ/kg-K"
        wUnits(1) = "m/s"
        visUnits(1) = "uPa-s"
        tcxUnits(1) = "W/m-K"
        stUnits(1) = "mN/m"
        'cgs (cgs)
        tUnits(2) = "K"
        taUnits(2) = "K"
        pUnits(2) = "MPa"
        dUnits(2) = "g/cm^3"
        vUnits(2) = "cm^3/g"
        hUnits(2) = "J/g"
        sUnits(2) = "J/g-K"
        wUnits(2) = "cm/s"
        visUnits(2) = "uPa-s"
        tcxUnits(2) = "mW/m-K"
        stUnits(2) = "dyn/cm"
        'English (E)
        tUnits(3) = "F"             'See comments above
        taUnits(3) = "R"
        pUnits(3) = "psia"
        dUnits(3) = "lbm/ft^3"
        vUnits(3) = "ft^3/lbm"
        hUnits(3) = "Btu/lbm"
        sUnits(3) = "Btu/lbm-R"
        wUnits(3) = "ft/s"
        visUnits(3) = "lbm/ft-s"
        tcxUnits(3) = "Btu/h-ft-F"
        stUnits(3) = "lbf/ft"
        'Molar English (molar E)
        tUnits(7) = "F"
        taUnits(7) = "R"
        pUnits(7) = "psia"
        dUnits(7) = "lbmol/ft^3"
        vUnits(7) = "ft^3/lbmol"
        hUnits(7) = "Btu/lbmol"
        sUnits(7) = "Btu/lbmol-R"
        wUnits(7) = "ft/s"
        visUnits(7) = "lbm/ft-s"
        tcxUnits(7) = "Btu/h-ft-F"
        stUnits(7) = "lbf/ft"
        'Mixed (M)
        tUnits(4) = "K"
        taUnits(4) = "K"
        pUnits(4) = "psia"
        dUnits(4) = "g/cm^3"
        vUnits(4) = "cm^3/g"
        hUnits(4) = "J/g"
        sUnits(4) = "J/g-K"
        wUnits(4) = "m/s"
        visUnits(4) = "uPa-s"
        tcxUnits(4) = "mW/m-K"
        stUnits(4) = "mN/m"
    End Sub

    Function ConvertUnitsRefProp(ByRef InpCode As String, ByRef Units As String, ByRef prop1 As Double, ByRef prop2 As Double)
        Dim i As Integer, at As String, bt As String, tConv As Double, DefaultUnits As Integer
        ConvertUnitsRefProp = Nothing
        If InpCode = Nothing Then InpCode = ""
        If Units = Nothing Then Units = ""
        If IsNothing(prop1) Then prop1 = 0
        If IsNothing(prop2) Then prop2 = 0
        If ierr > 0 Or ierr = -16 Then ConvertUnitsRefProp = Trim2(herr) : Exit Function
        If tUnits2 = "" Then
            Call SetupUnits(0)  'If Default units are changed, this needs to be called again.  Normally it is skipped after the first entry
        End If

        'Change the 0 in the following line to 3 for default English units, 1 for mks, or 2 for cgs, etc.

        DefaultUnits = 0
        i = DefaultUnits

        'Do not change the order of the next 7 statements
        If Left(UCase(Units), 2) = "SI" Then i = 0 'SI
        If UCase(Units) = "SI WITH C" Or UCase(Units) = "C" Then i = 5 'SI with C
        If Left(UCase(Units), 1) = "M" Then i = 4 'Mixed
        If UCase(Units) = "MOLAR SI" Then i = 6 'Molar SI
        If UCase(Units) = "MKS" Then i = 1 'mks
        If UCase(Units) = "CGS" Then i = 2 'cgs
        If Left(UCase(Units), 1) = "E" Then i = 3 'English
        If UCase(Units) = "MOLAR E" Then i = 7 'Molar English

        at = UCase(Left(InpCode, 1))
        bt = UCase(Mid(InpCode, 2, 1))
        'molmass = 1
        'If InStr(LCase(dUnits(i)), "mol") = 0 Then molmass = 2

        If at = "-" Then
            ConvertUnitsRefProp = prop1
            If prop1 >= -9999999 And prop1 <= -9999900 Then
                If prop1 = CLng(prop1) Then
                    ConvertUnitsRefProp = Trim2("Undefined")
                    Exit Function
                End If
            End If
            'If Len(Trim(Prop1)) > 0 Then
            If bt = "T" Then ConvertUnitsRefProp = UnitConvert(prop1, "T", tUnits2, tUnits(i))
            If bt = "A" Then ConvertUnitsRefProp = UnitConvert(prop1, "T", taUnits2, taUnits(i))
            If bt = "P" Then ConvertUnitsRefProp = UnitConvert(prop1, "P", pUnits2, pUnits(i))
            If bt = "D" Then ConvertUnitsRefProp = UnitConvert(prop1, "D", dUnits2, dUnits(i))
            If bt = "V" Then ConvertUnitsRefProp = UnitConvert(prop1, "D", vUnits2, vUnits(i))
            If bt = "H" Or bt = "E" Then ConvertUnitsRefProp = UnitConvert(prop1, "H", hUnits2, hUnits(i))
            If bt = "S" Then ConvertUnitsRefProp = UnitConvert(prop1, "S", sUnits2, sUnits(i))
            If bt = "W" Then ConvertUnitsRefProp = UnitConvert(prop1, "W", wUnits2, wUnits(i))
            If bt = "U" Then ConvertUnitsRefProp = UnitConvert(prop1, "U", visUnits2, visUnits(i))
            If bt = "K" Then ConvertUnitsRefProp = UnitConvert(prop1, "K", tcxUnits2, tcxUnits(i))
            If bt = "N" Then ConvertUnitsRefProp = UnitConvert(prop1, "N", stUnits2, stUnits(i))
            'End If
            If bt = "J" Then
                tConv = 1
                If tUnits(i) = "R" Or tUnits(i) = "F" Then tConv = 1 / RtoK
                ConvertUnitsRefProp = prop1 * tConv / UnitConvert(1, "P", "kPa", pUnits(i))
            End If
        Else
            If Len(Trim(prop1)) > 0 Then
                If at = "T" Then prop1 = UnitConvert(prop1, "T", tUnits(i), tUnits2)
                If at = "A" Then prop1 = UnitConvert(prop1, "T", taUnits(i), taUnits2)
                If at = "P" Then prop1 = UnitConvert(prop1, "P", pUnits(i), pUnits2)
                If at = "D" Then prop1 = UnitConvert(prop1, "D", dUnits(i), dUnits2)
                If at = "V" Then prop1 = UnitConvert(prop1, "D", vUnits(i), vUnits2)
                If at = "H" Or at = "E" Then prop1 = UnitConvert(prop1, "H", hUnits(i), hUnits2)
                If at = "S" Then prop1 = UnitConvert(prop1, "S", sUnits(i), sUnits2)
                If at = "W" Then prop1 = UnitConvert(prop1, "W", wUnits(i), wUnits2)
                If at = "U" Then prop1 = UnitConvert(prop1, "U", visUnits(i), visUnits2)
                If at = "K" Then prop1 = UnitConvert(prop1, "K", tcxUnits(i), tcxUnits2)
                If at = "N" Then prop1 = UnitConvert(prop1, "N", stUnits(i), stUnits2)
            End If

            If Len(Trim(prop2)) > 0 Then
                If bt = "T" Then prop2 = UnitConvert(prop2, "T", tUnits(i), tUnits2)
                If bt = "A" Then prop2 = UnitConvert(prop2, "T", taUnits(i), taUnits2)
                If bt = "P" Then prop2 = UnitConvert(prop2, "P", pUnits(i), pUnits2)
                If bt = "D" Then prop2 = UnitConvert(prop2, "D", dUnits(i), dUnits2)
                If bt = "V" Then prop2 = UnitConvert(prop2, "D", vUnits(i), vUnits2)
                If bt = "H" Or bt = "E" Then prop2 = UnitConvert(prop2, "H", hUnits(i), hUnits2)
                If bt = "S" Then prop2 = UnitConvert(prop2, "S", sUnits(i), sUnits2)
                If bt = "W" Then prop2 = UnitConvert(prop2, "W", wUnits(i), wUnits2)
                If bt = "U" Then prop2 = UnitConvert(prop2, "U", visUnits(i), visUnits2)
                If bt = "K" Then prop2 = UnitConvert(prop2, "K", tcxUnits(i), tcxUnits2)
                If bt = "N" Then prop2 = UnitConvert(prop2, "N", stUnits(i), stUnits2)
            End If
        End If
    End Function

    Function FluidString(ByVal Nmes, ByVal Comps) As String
        Dim a As String, i As Integer
        If Nmes.Count <> Comps.Count Then FluidString = "Number of fluid names and compositions not the same" : Exit Function
        a = ""
        For i = 0 To Nmes.Count
            If Nmes(i) <> "" Then a = a & Nmes(i) & ";" & Comps(i) & ";"
        Next
        Do While Right(a, 1) = ";"
            a = Left(a, Len(a) - 1)
        Loop
        FluidString = a
    End Function

    Function RefpropXLSVersionNumber()
        RefpropXLSVersionNumber = "8.01c"
    End Function



End Module


