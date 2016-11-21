Imports System
Imports System.Runtime.InteropServices

Public Module Wet_reduction

    Const Wreduction = "Wet_reduction.dll"
    'Declare external functions using the DllImport attribute.
    <DllImport(Wreduction, EntryPoint:="power",
    CallingConvention:=CallingConvention.StdCall)>
    Public Function power(ByVal X As Double) As Double
    End Function

    <DllImport(Wreduction, EntryPoint:="I0",
    CallingConvention:=CallingConvention.StdCall)>
    Public Function I0(ByVal X As Double) As Double
    End Function

    <DllImport(Wreduction, EntryPoint:="I1",
    CallingConvention:=CallingConvention.StdCall)>
    Public Function I1w(ByVal X As Double) As Double
    End Function

    <DllImport(Wreduction, EntryPoint:="K0",
    CallingConvention:=CallingConvention.StdCall)>
    Public Function K0(ByVal X As Double) As Double
    End Function

    <DllImport(Wreduction, EntryPoint:="K1",
    CallingConvention:=CallingConvention.StdCall)>
    Public Function K1(ByVal X As Double) As Double
    End Function

    <DllImport(Wreduction, EntryPoint:="fin_efficiency",
    CallingConvention:=CallingConvention.StdCall)>
    Public Function fin_efficiency(ByVal X As Double) As Double
    End Function

End Module