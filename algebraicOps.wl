(* ::Package:: *)

(* ::Section:: *)
(*Equations*)


min[x_]:=Module[{data},
data = Cases[x,_?NumericQ];
If[Length[data]>0,Min[data],Null]

]


max[x_]:=Module[{data},
data = Cases[x,_?NumericQ];
If[Length[data]>0,Max[data],Null]

]


(*total: List \[Rule] number*)
total[x_]:=Module[{data},
	data = Cases[x,_?NumericQ];
	  If[Length[data]>0,Total[data],Null]
]


(*round: float \[Rule] float*)
round[x_] := If[NumericQ[x],Round[x,0.0001], Null]


(*Average :    List<numbers (could have strings)> \[Rule] number  *)
Average[x_] := round[Mean [ Cases[x,_?NumericQ]]]


(*StandardDev :    List<numbers (could have strings)> \[Rule] number  *)
StandardDev[x_]:= Module[{tmp},
						tmp = Cases[x,_?NumericQ];
                                                       If[Length[tmp]>1,round[StandardDeviation[tmp]],Null]
				]


(*Ratio: {float,float} \[Rule] number*)
Ratio[{x_,y_}] := If[NumericQ[x]&&NumericQ[y],round[x/y],Null]


Ratio2[x_,y_]:=If[NumericQ[x]&&NumericQ[y],round[x/y],Null]


fCompareDate[date1_,date2_]:=Module[{list1, list2,n1,n2},
	list1 = StringSplit[date1,"-"];
	list2 = StringSplit[date2,"-"];
	n1 = ToExpression[list1[[1]]]*10000 + ToExpression[list1[[2]]]*100 +  ToExpression[list1[[3]]];
	n2 = ToExpression[list2[[1]]]*10000 + ToExpression[list2[[2]]]*100 +  ToExpression[list2[[3]]];
	n1>= n2 
]


fCompareDateStrict[date1_,date2_]:=Module[{list1, list2,n1,n2},
	list1 = StringSplit[date1,"-"];
	list2 = StringSplit[date2,"-"];
	n1 = ToExpression[list1[[1]]]*10000 + ToExpression[list1[[2]]]*100 +  ToExpression[list1[[3]]];
	n2 = ToExpression[list2[[1]]]*10000 + ToExpression[list2[[2]]]*100 +  ToExpression[list2[[3]]];
	n1> n2 
]


(*MA: [List<numeric>, integer] \[Rule] List<numeric>*)
MA[list_,MADays_]  :=If[Length[list]>0,MovingAverage[list,MADays ],{}];


fAverageScore[x_]:=Average[{x[["nuCPC score"]],x[["nuCPA score"]],x[["666 score"]],x[["budget spend score"]],x[["budget survival score"]]}];


fIsWeekend[date_]:=Module[{weekDay,isWeekend},
weekDay =DateString[DateObject[date],"DayName"];
isWeekend  = (weekDay=="Saturday")||(weekDay=="Sunday");
isWeekend
]


AddDays[date_, days_]:= DateString[DatePlus[date, days], {"Year","-","Month","-","Day"}];
AddMonths[date_, months_]:= DateString[DatePlus[date, {months, "Month"}], {"Year","-","Month","-","Day"}];
FirstOf[date_] := DateString[DateValue[date, {"Year", "Month"}], {"Year", "-", "Month", "-", "01"}]
