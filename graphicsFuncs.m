(* ::Package:: *)

BeginPackage["AnomalyDetection`"]


(* ::Section:: *)
(*Signatures*)


fListPlot::usage="fListPlot[data, legends,imageSize,plotTitle,labels]  returns Scatter plots of data sets";


fDateListPlot::usage="fListPlot[data, legends,imageSize,plotTitle,labels]  returns TS list plot on data sets";


fDateListPlot::usage="fBuildPieChart[list, legends, title,imageSize] return pie chart on data set";


fBuildHistograms::usage= "fBuildHistograms[data,interval,title,labels,legends,size] return histogram chart on dataset";


(* ::Section:: *)
(**)


Begin["Private`"]


(* ::Section::Closed:: *)
(*Setup*)


Get["Units`"];


(* ::Section::Closed:: *)
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


Percentage[x_] := Convert[x, Percent]/.Percent->"%"


(* ::Section:: *)
(*List plot*)


fListPlot[data_, legends_,imageSize_,plotTitle_,labels_]:=Module[{size,graphs,ymax,ymin,graphData},
	size= Length[data];
	ymax = max[Flatten[data]]*1.1;
	ymin  =min[Flatten[data]]*0.9;
	graphData=((Tooltip [#,#]&)/@#)&/@data;
	graphs= ListPlot[graphData,
					Mesh->All,
					MeshStyle->Directive[PointSize[Small],Black], 
					PlotLegends->legends,
					ImageSize->imageSize,
					LabelStyle->{Bold,Black,12},
					Frame->True,
					FrameLabel->labels,
					PlotRange->All ,PlotTheme->"Detailed",PlotLabel->plotTitle];
	graphs
]


(* ::Section:: *)
(*Date list plot*)


fDateListPlot[data_, legends_,imageSize_,plotTitle_,labels_]:=Module[{size,graphs,ymax,ymin,graphData},
	graphData=((Tooltip [#,#]&)/@#)&/@data;
	graphs = DateListPlot[graphData,
							Mesh->All,
							MeshStyle->Directive[PointSize[Small],Black], 
							PlotLegends->legends, 
							ImageSize->imageSize,
							LabelStyle->{Bold,Black,12}, 
							Frame->True,FrameLabel->labels,
							PlotRange->Full,PlotLabel-> plotTitle,PlotTheme->"Detailed"];
	Return[graphs]
]


(* ::Section:: *)
(*Pie chart*)


(* ::Input:: *)
(*fBuildPieChart[list_, legends_, title_,imageSize_]:=Module[{sumValue,percentage,graph},*)
(*	sumValue = total[list];*)
(*	percentage = Percentage/@(Round[#,0.001]&/@(N/@list/total[list]));*)
(*	graph =PieChart[list,ChartLabels->percentage,ChartLegends->legends,PlotLabel->title,LabelStyle->{12,Bold,Black},ImageSize->imageSize];*)
(*	graph	*)
(*]*)


(* ::Section:: *)
(*Histogram*)


(* ::Input:: *)
(*(*fHistographData: [List<numeric>,List<numeric>] \[Rule] List<numeric>*)*)
(*fHistographData[data_,range_]:= Module[*)
(*{tmp,f1,f2},*)
(*f1[x_]:=If[x>= range[[2]],range[[2]]-range[[3]],x];*)
(*f2[x_]:=If[x< range[[1]],range[[1]],x];*)
(*tmp = f1/@data ;*)
(*tmp = f2/@tmp;*)
(*tmp *)
(*]*)


(* ::Input:: *)
(*fBuildHistograms[data_,interval_,title_,labels_,legends_,size_]:=Module[*)
(*{histogramData,histoMin,histoMax,histoStepSize,graph,legendList},*)
(*histoMin  =interval[[1]];*)
(*histoMax  =  interval[[2]];*)
(*histoStepSize  = (histoMax - histoMin )/20;*)
(*histogramData=fHistographData[#,{histoMin,histoMax,histoStepSize}]&/@ data;*)
(*legendList="\nn: "<>ToString[Length[#]]    <> "\n\[Mu]: "<>ToString[Average[#]]<>"\n\[Sigma]: "<>ToString[StandardDev[#]]&/@data;*)
(**)
(*legendList= #[[1]]<>#[[2]]&/@Thread[List[legends,legendList]];*)
(**)
(*graph  = Histogram[histogramData,{histoMin,histoMax,histoStepSize},PlotLabel->Style[title ,FontSize->12, Bold],ChartLegends->legendList, LabelStyle->{Bold,Black,FontSize->12},Frame -> True, FrameLabel->labels,ImageSize->size];*)
(**)
(*graph*)
(**)
(*]*)


(* ::Section:: *)
(**)


End[]
EndPackage[]
