Imports System
Imports System.Runtime.InteropServices

Public Class Wet_reduction

	const Wreduction = "Wet_reduction.dll"
'Declare external functions using the DllImport attribute.
<DllImport(Wreduction, EntryPoint:= "power", _
	callingConvention:= callingConvention.stdCall)> _
	Public Shared Function power(byval X as double) as double
	End Function
	
<DllImport(Wreduction, EntryPoint:= "I0", _
	callingConvention:= callingConvention.stdCall)> _
	Public Shared Function I0(byval X as double) as double
	End Function

<DllImport(Wreduction, EntryPoint:= "I1", _
	callingConvention:= callingConvention.stdCall)> _
	Public Shared Function I1(byval X as double) as double
	End Function

<DllImport(Wreduction, EntryPoint:= "K0", _
	callingConvention:= callingConvention.stdCall)> _
	Public Shared Function K0(byval X as double) as double
	End Function

<DllImport(Wreduction, EntryPoint:= "K1", _
	callingConvention:= callingConvention.stdCall)> _
	Public Shared Function K1(byval X as double) as double
	End Function

<DllImport(Wreduction, EntryPoint:= "fin_efficiency", _
	callingConvention:= callingConvention.stdCall)> _
	Public Shared Function fin_efficiency(byval X as double) as double
	End Function

End Class