(* ::Package:: *)

ClearAll @@ ToExpression@Evaluate@Select[Names["Global`*"],StringContainsQ[#,"spd"]&];


spdPlot::usage = "Speed Plot
The clean, user-friendly plot function.
Inspired by Detailed Plot and Joseph's GigaChadPlot.
Check out https://rotaryviper.github.io/JMSS-Mathematica-Hub/ for other mathematica functions.

You can do whatever you want with this code. Use it, fix it, edit it, share it. This is yours to keep :)

How to use:
	Initialise the functions
		1. Open the spdFuncs notebook.
		2. Click within the notebook code cell.
		3. Shift + Enter to initialise the functions.

	Using the functions
		1. Click into any other notebook.
		2. Type: spdPlot[x]
		3. Shift + Enter to run the function.
		4. Watch magic happen :)
		5. Check/Uncheck the checkboxes on the left as you desire.

	Find Function
		1. Type any number or numeric symbol you want. Eg: 2, -\[ExponentialE], \!\(\*FractionBox[\(\[Pi]\), \(2\)]\), 5\!\(\*SqrtBox[\(3\)]\)
		2. Click enter.
		3. Dots will appear on the X or Y coordinate on any graph.
Syntax:
	Its the exact same as the mathematica Plot function.

Note:
	Type smaller equations are on the left, bigger equations are on the right. It runs faster that way.
	This plot function can only calculate real numbers.
	I tried keeping the function as similar to Plot as possible. But I'm still improving everyday.
	For users before v19, Incremental has been depreceated.


Sharing is caring - Rotary Viper";spdInverse::usage="Find Inverse
spdInverse[ expr_ , dom_:x ]

Enter a single expression into expr_
(Optional)Enter a symbol into dom_ to use as the domain

Swap the x and y around";spdFindX::usage="Find X
spdFindX[ equ_ , xCoord_ , dom_:x , rangeLow_:-\[Infinity] , rangeHigh_:\[Infinity] ]

Enter a single expression or list of expressions into equ_
Enter a number into xCoord_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a number into rangeLow_ to use as the lower range
(Optional)Enter a number into rangeHigh_ to use as the upper range

Sub the number into the expression(s)
FullForm:
Select[Flatten@Cases[{equ/.dom->xCoord},_?NumericQ],FreeQ[#, _Complex]&&rangeLow<=#<=rangeHigh&]
SimpleForm:
equ/.dom->xCoord";spdFindY::usage="Find Y
spdFindY[ equ_ , yCoord_ , dom_:x , domLow_:-10 , domHigh_:10 ]

Enter a single expression or list of expressions into equ_
Enter a number into yCoord_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a number into domLow_ to use as the lower domain
(Optional)Enter a number into domHigh_ to use as the upper domain

Solve for domain with the y coordinate and equation
FullForm:
Solve[equ==yCoord&&domLow<=dom<=domHigh,dom,Reals]
SimpleForm:
Solve[equ==yCoord,dom,Reals]";spdFindGrad::usage="Find Gradient
spdFindGrad[ equ_ , gradient_ , dom_:x , domLow_:-10 , domHigh_:10 , rangeLow_:-\[Infinity] , rangeHigh_:\[Infinity] ]

Enter a single expression or list of expressions into equ_
Enter a number into gradient_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a number into domLow_ to use as the lower domain
(Optional)Enter a number into domHigh_ to use as the upper domain
(Optional)Enter a number into rangeLow_ to use as the lower range
(Optional)Enter a number into rangeHigh_ to use as the upper range

Solve for x and y coordinates when derivative=0 and domain and range are within their allocated constraints
FullForm:
Solve[D[equ,dom]==gradient&&equ==y&&domLow<=dom<=domHigh&&rangeLow<=y<=rangeHigh,{dom,y},Reals]
SimpleForm:
Solve[D[equ,dom]==gradient&&equ==y,{dom,y},Reals]";spdPOI::usage="Find Points of Intersection
spdPOI[ equ_ , dom_:x , domLow_:-10 , domHigh_:10 , rangeLow_:-\[Infinity] , rangeHigh_:\[Infinity] ]

Enter a list of expressions into equ_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a number into domLow_ to use as the lower domain
(Optional)Enter a number into domHigh_ to use as the upper domain
(Optional)Enter a number into rangeLow_ to use as the lower range
(Optional)Enter a number into rangeHigh_ to use as the upper range

Solve for x and y coordinates of every expression provided and domain and range are within their allocated constraints
Select[{dom,y}/.Solve[equList[[equ1]]==equList[[equ2]]==y&&domLow<=dom<=domHigh&&rangeLow<=y<=rangeHigh,{dom,y},Reals],NumericQ[#[[1]]]&];
SimpleForm:
Solve[equList[[equ1]]==equList[[equ2]]==y,{dom,y},Reals]";spdAsyV::usage="Vertical Asymptotes
spdAsyV[ equList_ , dom_:x , domLow_:-10 , domHigh_:10 ]

Enter a list of expressions into equList_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a number into domLow_ to use as the lower domain
(Optional)Enter a number into domHigh_ to use as the upper domain

2 methods of finding vertical asymptotes are used:
Mathematica's way
FullForm:
1. Table[FunctionDomain[{equList[[$i]],domLow-1<=dom<=domHigh+1},dom],{$i,Length[equList]}]
SimpleForm:
FunctionDomain[equ,dom]
How to do it mathematically
2. Solve[1/equList[[$i]]==0&&domLow<=dom<=domHigh]

Combine the results of both then delete duplicates";spdAsyO::usage="Other Asymptotes
spdAsyO[ equList_ , dom_:x ]

This use to be the most broken speed function
Now resource function is used instead

Enter a list of expressions into equList_
(Optional)Enter a symbol into dom_ to use as the domain

FullForm:
Table[Simplify@PolynomialQuotient[Numerator[equList[[$i]]],Denominator[equList[[$i]]],dom],{$i,Length[equList]}]
SimpleForm:
PolynomialQuotient[Numerator[equ],Denominator[equ],dom]";spdFindTangent::usage="Find Tangent for given x
spdFindTangent[ equList_ , xCoord_ , dom_:x ]

Enter a list of expressions into equList_
Enter a number into xCoord_
(Optional)Enter a symbol into dom_ to use as the domain

Find gradient by subbing xCoord into derivative of expression
grad=D[equ,dom]/.dom->xCoord
Find yCoord by subbing xCoord into original expression
yCoord=equ/.dom->xCoord
Find tangent by applying the gradient along with horizontal and vertical translations until both equations touch once
tangent=grad(dom-xCoord)+yCoord

This is inverse Find Normal";spdFindNormal::usage="Find Normal for given x
spdFindNormal[ equList_ , xCoord_ , dom_:x ]

Enter a list of expressions into equList_
Enter a number into xCoord_
(Optional)Enter a symbol into dom_ to use as the domain

Find gradient by subbing xCoord into derivative of expression
grad=-1/D[equ,dom]/.dom->xCoord
Find yCoord by subbing xCoord into original expression
yCoord=equ/.dom->xCoord
Find tangent by applying the gradient along with horizontal and vertical translations until both equations touch once
tangent=grad(dom-xCoord)+yCoord

This is inverse Find Tangent";spdEndPoints::usage="Find endpoints for given x
spdFindNormal[ equ_ , dom_:x , domLow_:-10 , domHigh_:10 , rangeLow_:-10 , rangeHigh_:10 ]

Enter a list of expressions into equ_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a number into domLow_ to use as the lower domain
(Optional)Enter a number into domHigh_ to use as the upper domain
(Optional)Enter a number into rangeLow_ to use as the lower range
(Optional)Enter a number into rangeHigh_ to use as the upper range

Solve top, bottom, left and right walls

This is find edges and ";spdHoles::usage="Yes";spdCompleteTheSquare::usage="This function gives you the completed square form of any 3 terms.
The formula used is: \!\(\*SuperscriptBox[\(ax\), \(2\)]\)+bx+c.
Put your terms in the corresponding symbols.
spdCompleteTheSquare[a,b,c,False]

The final argument may be True or False. This chooses if the function gives you steps on how to complete the square.";spdPlotFast::usage="Foundation plot function for spdPlot
Syntax:
Its the exact same as the mathematica Plot function.";spdPlotIntegrate::usage="Plot integal of given expression(s)
Syntax:
Its the exact same as the mathematica Plot function.";spdPlotDerivative::usage="Plot derivative of given expression(s)
Syntax:
Its the exact same as the mathematica Plot function.";spdPlotInverse::usage="Plot inverse of given expression(s)
Syntax:
Its the exact same as the mathematica Plot function.
Please use:
spdInverse[equ]
To find answers";spdPlotCore::usage="spdPlot for core math.
It calculates:
X & Y intercepts
Turning Points
Horizontal & Vertical asymptotes
Syntax:
Its the exact same as the mathematica Plot function.";spdQuestionDerivatives::usage="Make a list of fast derivative questions.
spdQuestionDerivatives[ equ_ , dom_:x , range_:Range[20] , length_:45 , row_:4 , name_: ''Derivates Speed Run'' ]

The default length and row are perfect for making the most out of each a4 piece of paper

Enter a list of expressions into equ_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a list of numbers into range_ to sub into the symbols that aren't domain in the list of expressions
(Optional)Enter a number into length_ for each run
(Optional)Enter a number into row_ for the number of runs
(Optional)Enter a string into name_ to name runs";


(*Copy equation*)
spdCopyThis[equ_,message_:spdStyle["Copy This"]]:=Button[message,CopyToClipboard[equ]];
(*Tells you an error*)
spdErrorChecker=Quiet@Check[#,spdStyle@"Error"]&;

(*-------------------------Quality of life functions-------------------------*)
(*Execute a new mathematica process*)
spdNewNoteBook:=SystemOpen[FileNameJoin[{$InstallationDirectory,"Mathematica"}]];

(*Convert equation into clean TraditionalForm*)
spdForm[expr_]:=StandardForm@TraditionalForm@expr;

(*-------------------------Global constants-------------------------
Customise these to your heart's content.
Restart mathematica when changing these options, or they many not apply.
*)
spdFont="Nunito";
spdColourPlot=Automatic;
spdColourPoint1=Red;
spdColourPoint2=Green;
spdColourPoint3=Blue;
spdColourPoint4=Black;
spdColourLine=Red;
spdRoundPrecision=MachinePrecision;
spdMessage="Sharing is Caring";
spdPlotSize=Medium;
spdPlotRange=Automatic;(*Just like a persistent PlotRange*)
spdDragSens=.003;

spdPlotFaster[equ_,dom:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotOptions:OptionsPattern[]]:=
Plot[Evaluate@Flatten@{equ},dom,Evaluate@FilterRules[{plotOptions},Options@Plot],Background->spdColourPlot,AspectRatio->1,PlotLegends->"Expressions",ImageSize->spdPlotSize,PlotLabel->spdStyle[spdMessage]];

(*-------------------------Processing-------------------------
Enter any formula, points then get the graph
List of formulas*)

spdLinear=a x+b;spdQuad=a x^2+b x+c;spdCubic=a x^3+b x^2+c x+d;spdQuartic=a x^4+b x^3+c x^2+d x+e;

spdEpicyclePlay[img_,colour_:Automatic]:=
Module[{a=spdEpicycleCore[img,colour]},(*spdEpicyclePlay[img,colour]=*)ListAnimate[Flatten@{a,Table[a[[-1]],20],Reverse@a},60]
];
spdEpicycleGIF[img_,colour_:Automatic]:=
Module[{a=spdEpicycleCore[img,colour]},(*spdEpicycleGIF[img,colour]=*)Export["epicycle.gif",Flatten@{a,Table[a[[-1]],20],Reverse@a},"DisplayDurations" ->1/60]
];



(*Enter any formula, points then get the graph*)
spdFormula[formula_,points_,x_:x]:=
With[
	{
		(*Find all the variables*)
		$variables=
		Complement[
			DeleteDuplicates@Cases[formula,_Symbol,Infinity],
			{x}
		]
	},
	Unprotect[spdFormula];
	(*Sub values into variables of original formula*)
	spdFormula[formula,points,x]=
		spdEquPreview
		[
			formula/.
				Solve
				[
					Table
					[
						(formula/.x->points[[point]][[1]])==points[[point]][[2]],
						{point,Length[points]}
					],
				$variables,Reals
				][[1]]
		]
];

(*Enter any formula, points then get the graph*)
spdFormula2[formula_,points_,x_:x]:=
With[
	{
		(*Find all the variables*)
		variables=
		Complement[
			DeleteDuplicates@Cases[formula,_Symbol,Infinity],
			{x}
		]
	},
	Unprotect[spdFormula2];
	(*Sub values into variables of original formula*)
	spdFormula2[formula,points,x]=
		spdEquPreview[
			Simplify[
				formula/.
					Solve[
						points,
						variables,Reals
					]
			]
		]
];


(*-------------------------StartUp quality of life things-------------------------*)
(*Functions to make listable*)
SetAttributes[{spdFindX,spdFindY,spdFindGrad,spdFindTangent,spdFindNormal,spdEndPoints,spdHoles,spdStyle,spdDesectFunction,spdEquPlotG,spdEquPlotGInverse,spdInverse,spdAsyO},Listable];
SetAttributes[{spdSwapHeads,spdPlot,spdPlotCore,spdQuestionDerivatives},HoldAll];
(*Protect certain values for spdPlot*)
(*
spdPlotVariables={equList,equInput,initPos,finalPos,idomLow,idomHigh,domLow,domHigh,irangeLow,irangeHigh,rangeLow,rangeHigh,findXInput,findXBool,findXG,findYInput,findYBool,findYG,findGradInput,findGradBool,findTangentInput,findGradG,findTangentInput,findTangentBool,findTangentG,findNormalInput,findNormalBool,findNormalG,asyBool,asyG,poiBool,poiG,epBool,epG,inverseBool,inverseG,derivativeBool,derivativeG,integrateBool,integrateG};
*)
(*-------------------------Small functions-------------------------*)
spdAltPlotLabel=PlotLabels->Placed[
(*If there is more than one expression, use a list. Otherwise do not use a list*)
	If[#2>1,
		Table[spdStyle@#,#2],
		spdStyle@#
	],
{{Scaled[.9],Before}}
]&;
(*Placed[Table[spdStyle@"Derivative",5],{{Scaled[.9],Before}}]*)

spdInputField=InputField[#1,Evaluate@FilterRules[{FrameMargins->None,ContentPadding->False,Alignment->{Left,Center},FieldSize->#2},Options@InputField]]&;

spdAsyParser=Map[#[[1]][[2]][[2]]&,Evaluate@#]&;

spdDesectFunction[expr_]:={
	Module[{
		(*Check head of expression*)
		context=Context@Evaluate@Head@Unevaluated@expr,
		head=Evaluate@Head@Unevaluated@expr
	},
		(*If head of expression is in the global context*)
		If[context==="Global`",
		Return[Extract[DownValues[Evaluate@OwnValues[head][[1]][[2]]],{1,2},Unevaluated],spdDesectFunction],
		Return[Unevaluated@Unevaluated@expr,spdDesectFunction]
		]
	]
}


(*I didn't write any of this. Thank you Tim*)
spdEpicycleCore[img_,colour_:Automatic]:={
Module[{img1=img,pts,center,toPt,cf,z,m,cn,f,g,r,theta,index,p,circles,anims,gif},{
img1 = Binarize[img1~ColorConvert~"Grayscale"~ImageResize~500~Blur~3];
pts = DeleteDuplicates@
   Cases[Normal@
      ListContourPlot[Reverse@ImageData[img1], 
       Contours -> {0.5}], _Line, -1][[1, 1]];
center = Mean@MinMax[pts] & /@ Transpose@pts;
pts = # - center & /@ pts[[;; ;;20]];
SetAttributes[toPt,Listable];
toPt[z_]:=ComplexExpand[{Re@z,Im@z}]//Chop;
cf=Compile[{{z,_Complex,1}},Module[{n=Length@z},1/n*Table[Sum[z[[k]]*Exp[-$i*$i*k*2 Pi/n],{k,1,n}],{$i,-m,m}]],RuntimeOptions->"Speed"];
z=pts[[All,1]]+$i*pts[[All,2]];
m=60;
cn=cf[z];
{f[t_],g[t_]}=Sum[cn[[j]]*Exp[$i*(j-m-1)*t],{j,1,2 m+1}]//toPt;
r = Abs /@ cn;
theta = Arg /@ cn;
index = {m + 1}~Join~
   Riffle[Range[m + 2, 2 m + 1], Reverse[Range[1, m]]];
p[t_] = Accumulate@Table[cn[[j]]*Exp[$i*(j - m - 1)*t], {j, index}] // toPt;
circles[t_] = 
  Table[Circle[p[t][[$i-1]], r[[index[[$i]]]]], {$i, 2, 2 m + 1}];
anims = ParallelTable[
   ParametricPlot[{f[s], g[s]}, {s, 0, t}, AspectRatio -> Automatic,
     Epilog -> {circles[t], Line[p[t]], Point[p[t]]}, 
     PlotRange -> {{-250, 250}, {-250, 250}}, 
     ImageSize -> 500,Axes->False,PlotStyle->colour],
   {t, Subdivide[0.1, 2 Pi, 100]}];
(*spdEpicycleCore[img,colour]=*)gif=Flatten@{anims,Table[anims[[-1]],20],Reverse@anims};
Return[
gif,
spdEpicycleCore]
}]
};



(*Inverse any equation*)
spdInverse[expr_,dom_:x]:={
Module[{result=spdErrorChecker@Unevaluated@TableForm[Simplify[y/.Solve[dom==(expr/.dom->y),y,Reals]]]},
Unprotect[spdInverse];
spdInverse[Unevaluated@expr,dom]=result;
Protect[spdInverse];
Return[result,spdInverse]
]
};


(*Find any X point on graph*)
spdFindX[equ_,xCoord_,dom_:x,rangeLow_:-\[Infinity],rangeHigh_:\[Infinity]]:=
{
	Quiet@Module[
		{
			yCoord,
			result={}
		},
		{
		(*Compute yCoord, and check if yCoord is even on the graph
			Select removes complex numbers*)
			yCoord=
			Select[
				Flatten@Cases[
					Quiet@{equ/.dom->xCoord},_?NumericQ
				],
				FreeQ[
				#, _Complex
				]&&
					Floor@rangeLow<=#<=Ceiling@rangeHigh
			&];
		(*Return something if yCoord is not empty*)
		If[
			yCoord=={},
			{},
			(*Return all xCoords with yCoords. Delete duplicates
			Include gradient
			Caches results for spdPlot*)
			result=
				Simplify@
				{
					Defer@equ,DeleteDuplicates@
					Table[
						{
							xCoord,yCoord[[$i]],
							D[equ,dom]/.dom->xCoord
						},
						{$i,Length[yCoord]}
					]
				}
		],
		Unprotect[spdFindX];
		spdFindX[Unevaluated@equ,xCoord,dom,rangeLow,rangeHigh]=result;
		Protect[spdFindX];
		Return[result,spdFindX]
		}
	]
};


(*Find any Y point on graph*)
spdFindY[equ_,yCoord_,dom_:x,domLow_:-10,domHigh_:10]:=
{
	(*Find any point on the graph*)
	Quiet@Module[
		{
			(*Compute xCoord, and check if xCoord is even on the graph*)
			xCoord=
				Flatten@
					Cases[
						dom/.
							Solve[
								equ==yCoord&&
											Floor@domLow<=dom<=Ceiling@domHigh,
								dom,Reals
							],
						_?NumericQ
					],
			result={}
		},
		{
			(*Return something if xCoord is not empty*)
			If[
				xCoord=={},
				{},
				(*Return all xCoords with yCoords. Delete duplicates
				Caches results for spdPlot*)
				result=
				{
					Defer@equ,Simplify@DeleteDuplicates@
					Table[
						{
							xCoord[[$i]],
							yCoord,
							D[equ,dom]/.dom->xCoord[[$i]]
						},
						{$i,Length[xCoord]}
					]
				};
			];
			Unprotect[spdFindY];
			spdFindY[Unevaluated@equ,yCoord,dom,domLow,domHigh]=result;
			Protect[spdFindY];
			Return[result,spdFindY]
		}
	]
};


(*Find gradients*)
spdFindGrad[equ_,gradient_,dom_:x,domLow_:-10,domHigh_:10,rangeLow_:-\[Infinity],rangeHigh_:\[Infinity]]:=
{
	Quiet@Module[
		{
			cp,
			result,
			y
		},
		{
			(*Iterate over all lists*)
			cp=
			Select[
				{dom,y}/.
					Solve[
						D[equ,dom]==gradient&&
							equ==y&&
								Floor@domLow<=dom<=Ceiling@domHigh&&
									Floor@rangeLow<=y<=Ceiling@rangeHigh,
						{dom,y},Reals
					],
				NumericQ[#[[1]]]
			&];
			(*Output cp X and Y, along with the equation
			Caches results for spdPlot*)
			result=
			Simplify@
			{
				Defer@equ,
				Table[
					Flatten@
					{
						cp[[$i]],
						gradient
					},
					{$i,Length[cp]}
				]
			};
			Unprotect[spdFindGrad];
			spdFindGrad[Unevaluated@equ,gradient,dom,domLow,domHigh,rangeLow,rangeHigh]=result;
			Protect[spdFindGrad];
			Return[result,spdFindGrad]
		}
	]
};


(*Find points of intersection*)
spdPOI[equList_,dom_:x,domLow_:-10,domHigh_:10,rangeLow_:-\[Infinity],rangeHigh_:\[Infinity]]:=
{
	(*Find any point on the graph*)
	Quiet@Module[
		{
			poi,
			result,
			y,
			equListC=Table[Extract[Hold[equList],{1,i},Defer],{i,Length[equList]}]
		},
		{		
			(*Make sure equList is longer than 1*)
			If[
				Length[equList]>1,
				(*Calculate the intersection
				Caches results for spdPlot*)
				result=
				DeleteDuplicates@Flatten[
					(*Parallel Tables caused too much trouble*)
					Table[
						(*Plz stay silent, even in parallel processing*)
						Table[
							{
								(*Remove lines that don't intersect*)
								poi=
								Select[
									{dom,y}/.
										Solve[
											equList[[equ1]]==equList[[equ2]]==y&&
												Floor@domLow<=dom<=Ceiling@domHigh&&
													Floor@rangeLow<=y<=Ceiling@rangeHigh,
											{dom,y},Reals
										],
										NumericQ[#[[1]]]
								&];
								(*Some numbers are too big for plotting. Make them numeric if they are too big*)
								(*Include equations*)
								Simplify@
								{
									{
										equListC[[equ1]],
										equListC[[equ2]]
									},
									(*Include points*)
									Table[
										{
											poi[[$i]][[1]],
											poi[[$i]][[2]],
											(*Include gradients*)
											{
												(D[equList[[equ1]],dom])/.
													dom->poi[[$i]][[1]],
												(D[equList[[equ2]],dom])/.
														dom->poi[[$i]][[1]]
											}
										},{$i,Length[poi]}
									]
								}
							},
						{equ2,equ1+1,Length[equList]}],
					{equ1,Length[equList]}
					],
				2],
				(*If equList has a length of 1 or less, return nothing*)
				result={}
			];
			Unprotect[spdPOI];
			spdPOI[Unevaluated@equList,dom,domLow,domHigh,rangeLow,rangeHigh]=result;
			Protect[spdPOI];
			Return[result,spdPOI]
		}
	]
};



(*Find vertical asymptotes*)
spdAsyV[equList_,dom_:x,domLow_:-10,domHigh_:10]:=
{
	Quiet@Module[{
			lines,
			result
		},
		{
			(*Remove all non numbers*)
			lines=Select[
				Flatten@Table[
				dom/.Solve[
						1/equList[[$i]][[1]]==0&&Floor@domLow<=dom<=Ceiling@domHigh
					],
					{$i,Length[equList]}
				],
			NumericQ
			];
			(*Convert into useable points
			Caches results for spdPlot*)
			result=
			(*TooltipString*)
			Table[{
				{
					"Vertical Asymptote",
					{N[lines[[$i]],spdRoundPrecision],lines[[$i]]}
				},
				(*Points*)
				{{lines[[$i]],0},{lines[[$i]],1}}
			},
			{$i,Length[lines]}
			];
		Unprotect[spdAsyV];
		spdAsyV[equList,dom,domLow,domHigh]=result;
		Protect[spdAsyV];
		Return[result,spdAsyV]
		}
	]
};



(*Find other asymptotes*)
spdAsyO[equ_,dom_:x]:=
{
	Module[
		{
			result
		},
		{
			result=
			spdEquPlotG[
				(*Remove all the wrong asymptotes, non-asymptotes, duplicates*)
				DeleteCases[
					Complement[
						DeleteDuplicates@Flatten@
						{
							(*Just find the other asymptotes*)
							Map[$Noty/.#&,Flatten@Map[ResourceFunction["Asymptotes"][Unevaluated@equ,dom,$Noty,#]&,{"Oblique","Parabolic","Other"}]],
							(*Thank you for staying quiet*)
							Quiet@PolynomialQuotient[Numerator[Together@equ],Denominator[Together@equ],dom]
						},
						{$Noty,equ}
					],
					_PolynomialQuotient
				],
			"Asymptote",Column@{"Original Equation",Defer@equ},dom];
		Unprotect[spdAsyO];
		spdAsyO[Unevaluated@equ,dom]=result;
		Protect[spdAsyO];
		Return[result,spdAsyO]
		}
	]
};




(*Find tangent*)
spdFindTangent[equ_,xCoord_,dom_:x]:={
Quiet@Module[{
grad,
yCoord,
tangent,
result
},{
(*Find gradient*)
grad=D[equ,dom]/.dom->xCoord;
(*Find yCoord*)
yCoord=equ/.dom->xCoord;
(*Find tangent*)
tangent=grad(dom-xCoord)+yCoord;
If[
(*If y is indeterminate or infinity*)
And@@(FreeQ[{grad,yCoord,tangent},#]&/@{Indeterminate,ComplexInfinity,DirectedInfinity}),
result=Simplify@{
(*Convert into graphics*)
(*Create a string of data to display*)
(*TooltipString*)
{"Find Tangent",
{"Equation:",Defer@equ},
{"Tangent:",tangent},
{"Gradient:",grad}
},
{{0,tangent/.dom->0},{xCoord,yCoord}}
},
result={}];

Unprotect[spdFindTangent];
spdFindTangent[Unevaluated@equ,xCoord,dom]=result;
Protect[spdFindTangent];
Return[result,spdFindTangent]
}]
};


(*Find normal*)
spdFindNormal[equ_,xCoord_,dom_:x]:={
Quiet@Module[{
grad,
yCoord,
normal,
result
},{
(*Find gradient*)
grad=-1/D[equ,dom]/.dom->xCoord;
(*Find yCoord*)
yCoord=equ/.dom->xCoord;
(*Find normal*)
normal=grad(dom-xCoord)+yCoord;
If[
(*If y is indeterminate or infinity*)
And@@(FreeQ[{grad,yCoord,normal},#]&/@{Indeterminate,ComplexInfinity,DirectedInfinity}),
result=Simplify@{
(*Convert into graphics*)
(*Create a string of data to display*)
(*TooltipString*)
{"Find Normal",
{"Equation:",Defer@equ},
{"Tangent:",normal},
{"Gradient:",grad}
},
{{0,normal/.dom->0},{xCoord,yCoord}}
},
result={}];

Unprotect[spdFindNormal];
spdFindNormal[Unevaluated@equ,xCoord,dom]=result;
Protect[spdFindNormal];
Return[result,spdFindNormal]
}]
};


(*Find End Points*)
spdEndPoints[equ_,dom_:x,domLow_:-10,domHigh_:10,rangeLow_:-10,rangeHigh_:10]:={
Quiet@Module[{
rules=Dispatch[{Inequality->List,Or->List,Less->Nothing,LessEqual->List,Greater->Nothing,GreaterEqual->List,True->List,Null->List,Floor@domLow-1->Sequence,Ceiling@domHigh+1->Sequence}],
epX=FunctionDomain[Unevaluated@equ,dom,Reals],
epY,
edges,
result},
{
(*Find if edges exist using the solve method*)
edges=DeleteDuplicates@{
(*Left Wall*)
Map[{domLow,#,D[Unevaluated@equ,dom]/.dom->domLow}&,y/.Solve[y==Unevaluated@equ&&rangeLow<=y<=rangeHigh/.dom->domLow,y,Reals]],
(*Right Wall*)
Map[{domHigh,#,D[Unevaluated@equ,dom]/.dom->domHigh}&,y/.Solve[y==Unevaluated@equ&&rangeLow<=y<=rangeHigh/.dom->domHigh,y,Reals]],
(*Top Wall*)
Map[{#,rangeLow,D[Unevaluated@equ,dom]/.dom->#}&,dom/.Solve[rangeLow==Unevaluated@equ&&domLow<=dom<=domHigh,dom,Reals]],
(*Bottom Wall*)
Map[{#,rangeHigh,D[Unevaluated@equ,dom]/.dom->#}&,dom/.Solve[rangeHigh==Unevaluated@equ&&domLow<=dom<=domHigh,dom,Reals]]
};

(*Get every expression possible*)
epX=Level[epX,{0,\[Infinity]}];
(*Parse out everything not less or greater*)
epX=Table[Select[epX,Head@#===i&],{i,{LessEqual,GreaterEqual,Unequal}}];
(*Get only numbers from expression*)
epX=DeleteDuplicates@Select[Level[epX,{0,\[Infinity]}],NumericQ];
(*Find the y coords of each xCoord*)
epY=Table[Limit[equ,x->epX[[$i]]],{$i,Length[epX]}];

(*All accept lists that are 3 long*)
edges=Select[Flatten[edges,1],Length@#==3&];
(*Remove points if any of the 3 points are not numeric*)
edges=Select[edges,AllTrue[#,NumericQ]&];

(*Combine both methods of calculating points*)
edges=Flatten[{edges,Transpose@{epX,epY,Table["Indeterminate",Length[epX]]}},1];

result={{Defer@equ},edges};

Unprotect[spdEndPoints];
spdEndPoints[Unevaluated@equ,dom,domLow,domHigh,rangeLow,rangeHigh]=result;
Protect[spdEndPoints];
Return[result,spdEndPoints]
}]
};


(*Must be input with Unevaluated@expr*)
spdHoles[expr_,dom_:x]:={
Quiet@Module[{
	holeX=FunctionDomain[Unevaluated@expr,dom,Reals],
	holeY,
	rules=Dispatch[{\[Infinity]->Indeterminate,-\[Infinity]->Indeterminate}],
	result,
	combine
},{
	(*Get every expression possible*)
	holeX=Level[holeX,{0,\[Infinity]}];
	(*Parse out everything not less or greater*)
	holeX=Table[Select[holeX,Head@#===i&],{i,{Less,Greater}}];
	(*Get only numbers from expression*)
	holeX=DeleteDuplicates@Select[Level[holeX,{0,\[Infinity]}],NumericQ];

	(*If not a single x point is found, return nothing*)
	If[Flatten@holeX=={},Return[{},spdHoles]];
	
	(*Find the y coordinates that don't exist*)
	holeY=Map[Limit[expr,dom->#]&,holeX];
	
	(*Combine holeX and holeY*)
	combine=Transpose@{holeX,holeY,Table["Its a Hole",Length[holeX]]};
	
	(*Filter out all indeterminate and infinite points*)
	combine=Select[combine/.rules,#[[2]]=!=Indeterminate&];
	
	result={Defer@expr,combine};
	
	Unprotect[spdHoles];
	spdHoles[Unevaluated@expr,dom]=result;
	Protect[spdHoles];
	Return[result,spdHoles]
}]
};


(*Function for complete the square*)

spdCompleteTheSquare[a_,b_,c_,Optional[verbose_,False]]:={
If[verbose==True,
(*Declare complete the square string first*)
Module[{
cmpString={"Step 1: \!\(\*SuperscriptBox[\(ax\), \(2\)]\) + bx + c = 0",
HoldForm[x^2 + b x + c == 0],
"",
"Step 2: \!\(\*SuperscriptBox[\(x\), \(2\)]\) + \!\(\*FractionBox[\(b\), \(a\)]\)x + \!\(\*FractionBox[\(c\), \(a\)]\) = \!\(\*FractionBox[\(0\), \(a\)]\)",
HoldForm[x^2 + b/a x + c/a == 0],
"",
"Step 3: \!\(\*SuperscriptBox[\(x\), \(2\)]\) + \!\(\*FractionBox[\(b\), \(a\)]\)x = -\!\(\*FractionBox[\(c\), \(a\)]\)",
HoldForm[x^2 + b/a x == -(c/a)],
"",
"Step 4: \!\(\*SuperscriptBox[\(x\), \(2\)]\) + \!\(\*FractionBox[\(b\), \(a\)]\)x + (\!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\) = -\!\(\*FractionBox[\(c\), \(a\)]\) + (\!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\)",
HoldForm[x^2 + b/a x + (b/(2a))^2 == -(c/a) + (b/(2a))^2],
"",
"Step 5: (x + \!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\) = -\!\(\*FractionBox[\(c\), \(a\)]\) + (\!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\)",
HoldForm[(x + b/(2a))^2 == -(c/a) + (b/(2a))^2],
"",
"Step 6: a(x + \!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\) + c - \!\(\*FractionBox[SuperscriptBox[\(b\), \(2\)], \(4  a\)]\) = 0"}}
,{
(*Create Table of Styled string, that iterates over complete the square spring*)
Print[TableForm@Table[spdStyle[spdForm@cmpString[[$i]]],{$i,Length[cmpString]}]]
}]];
a (x+b/(2a))^2+c-b^2/(4a)==0//spdForm
}[[1]];
(*-------------------------Objects-------------------------
Compute points with tooltip*)
spdPointsG[points_,label_,colour_:spdColourPoint1]:=
Graphics[Table[Table[

(*Create a string of data to display*)
DynamicModule[{
tooltipString=spdStyle@Column@{label,
(*$i hate all of these brackets with all my heart,
and $i will never be able to debug this in the future*)
TableForm@{"Equation:",points[[point]][[1]]},
TableForm@{
{"X:",N[points[[point]][[2]][[point2]][[1]],spdRoundPrecision],Rationalize[points[[point]][[2]][[point2]][[1]]]},
{"Y:",N[points[[point]][[2]][[point2]][[2]],spdRoundPrecision],Rationalize@Rationalize[points[[point]][[2]][[point2]][[2]]]},
{"Grad:",N[points[[point]][[2]][[point2]][[3]],spdRoundPrecision],Simplify[points[[point]][[2]][[point2]][[3]]]}
}
}
},{
(*Event handler can handle clicks*)
EventHandler[
(*Convert into tooltip form*)
Tooltip[
{
{PointSize[Large],Darker@colour,Point[N@{points[[point]][[2]][[point2]][[1]],points[[point]][[2]][[point2]][[2]]}]},
{PointSize[Medium],Lighter@Lighter@colour,Point[N@{points[[point]][[2]][[point2]][[1]],points[[point]][[2]][[point2]][[2]]}]}
},
(*Give coordinate when you hover over location*)
tooltipString//spdForm
],
(*If a click is detected on point, print whatever is in tooltip*)
{"MouseClicked":>Print@Framed[tooltipString,Background->LightYellow]}]
}],

(*Iterate over every point*)
{point2,Length[points[[point]][[2]]]}],
(*Iterate over every equation*)
{point,Length[points]}]];


spdPointsGPOI[points_,label_,colour_:spdColourPoint1]:=
Graphics[Table[Table[

(*Create a string of data to display*)
DynamicModule[{
tooltipString=spdStyle[Column@{label,
(*$i hate all of these brackets with all my heart,
and $i will never be able to debug this in the future*)
TableForm@{
{"Equation:",Style[points[[point]][[1]][[1]],Background->Lighter@spdColourPoint1],Style[points[[point]][[1]][[2]],Background->Lighter@spdColourPoint2]},
{"X:",N[points[[point]][[2]][[point2]][[1]],spdRoundPrecision],Rationalize@points[[point]][[2]][[point2]][[1]]},
{"Y:",N[points[[point]][[2]][[point2]][[2]],spdRoundPrecision],Rationalize@points[[point]][[2]][[point2]][[2]]},
{"Grad:",Style[Column@{N[points[[point]][[2]][[point2]][[3]][[1]],spdRoundPrecision],Simplify@points[[point]][[2]][[point2]][[3]][[1]]},Background->Lighter@spdColourPoint1],Style[Column@{N[points[[point]][[2]][[point2]][[3]][[2]],spdRoundPrecision],Simplify@points[[point]][[2]][[point2]][[3]][[2]]},Background->Lighter@spdColourPoint2]}
}
}]
},{
(*Event handler can handle clicks*)
EventHandler[
(*Convert into tooltip form*)
Tooltip[
{
{PointSize[Large],Darker@colour,Point[N@{points[[point]][[2]][[point2]][[1]],points[[point]][[2]][[point2]][[2]]}]},
{PointSize[Medium],Lighter@Lighter@colour,Point[N@{points[[point]][[2]][[point2]][[1]],points[[point]][[2]][[point2]][[2]]}]}
},
(*Give coordinate when you hover over location*)
tooltipString//spdForm
],
(*If a click is detected on point, print whatever is in tooltip*)
{"MouseClicked":>Print@Framed[tooltipString,Background->LightYellow]}]
}],

(*Iterate over every point*)
{point2,Length[points[[point]][[2]]]}],
(*Iterate over every equation*)
{point,Length[points]}]];


(*Compute lines with tooltip*)
spdLinesG[points_,colour_:spdColourLine]:=
Graphics[Table[Table[

(*Create a string of data to display*)
DynamicModule[{
tooltipString=spdStyle[TableForm@points[[point]][[1]]]
},{
(*Event handler can handle clicks*)
EventHandler[
(*Convert into tooltip form*)
(*This is disgusting*)
Tooltip[{Dashed,colour,InfiniteLine[N@points[[point]][[2]]]},
(*Give coordinate when you hover over location*)
tooltipString//spdForm
],
(*If a click is detected on point, print whatever is in tooltip*)
{"MouseClicked":>Print@Framed[tooltipString,Background->LightYellow]}]
}],

(*Iterate over every line*)
{point2,Length[points[[point]][[2]]]}],
(*Iterate over every equation*)
{point,Length[points]}]];

(*Idk why this has to be evaluated
$i made it work but its messy code*)
spdEquPlotG[equ_,message_:"Equation",message2_:"",dom_:x]:=
{Module[{head,context},
{
If[Flatten@{equ}=={},
(*If equation is empty, make it return nothing*)
{},


(*Simple error checking*)
With[{equString=spdStyle[Column@{message,Extract[Hold@equ,1,Defer],message2,TableForm@{Column@{"Domain",spdErrorChecker@Unevaluated@Simplify@FunctionDomain[equ,dom,Reals]},Column@{"Range",spdErrorChecker@Unevaluated@Simplify@FunctionRange[equ,dom,Global`y,Reals]}}/.Dispatch[True->Reals]}]},
EventHandler[Tooltip[Evaluate@equ,equString//spdForm//ReleaseHold],
"MouseClicked":>Print@Framed[ReleaseHold@equString,Background->LightYellow]]]
]}]};

(*I hate this code but its necessary*)
spdEquPlotGInverse[equ_,message_:"Equation",message2_:"",dom_:x]:=
{Module[{head,context},
{
If[Flatten@{equ}=={},
(*If equation is empty, make it return nothing*)
{},
(*Simple error checking*)                           (*I hate this extract code but it works so well*)
With[{equString=spdStyle[Column@{message,message2,Extract[Hold@equ,1,Defer],TableForm@{Column@{"Domain",spdErrorChecker@Unevaluated@FunctionRange[equ,dom,Global`y,Reals]},Column@{"Range",spdErrorChecker@Unevaluated@FunctionDomain[equ,dom,Reals]}}/.Dispatch[True->Reals]}]},
EventHandler[Tooltip[{Evaluate@equ,dom},equString//spdForm],
"MouseClicked":>Print@Framed[ReleaseHold@equString,Background->LightYellow]]]
]}]};


(*Plot equations after hovering over them*)
spdEquPreview[equ_,size_:Small]:=TableForm[{Tooltip[spdForm@equ,spdPlotFast[equ,ImageSize->size]],spdCopyThis[equ]},TableDirections->Row];
(*Predefined style function*)
spdStyle=Style[#,FontFamily->spdFont,FontSize->Larger]&;
(*-------------------------Plot-------------------------*)
Options[spdPlot]=Options[Plot];


(*$i like this plot
$i don't care too much about commenting this but It Just Works*)
(*Table[spdEquPlotG[Flatten[{equ}][[$i]]],{$i,Length@Flatten@{equ}}]*)
spdPlotFast[equ_,dom:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotOptions:OptionsPattern[]]:=
Dynamic[
spdPlotFaster[Evaluate[spdEquPlotG[equ]], dom,
plotOptions
],
SynchronousUpdating->False,
TrackedSymbols:>{equ},
CachedValue->Once@ToBoxes[spdPlotFaster[equ,dom,plotOptions]]
];


spdPlotDerivative[equ_,dom:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotOptions:OptionsPattern[]]:={
Module[{
(*Turn equations into a standard table form that can be used*)
equList=Flatten@{equ},
equListD,
result
},{
equListD=Simplify@Map[D[#,dom[[1]]]&,equList];
result=Plot[Evaluate@Table[spdEquPlotG[Evaluate@equListD[[$i]],"Derivative",Column@{"Original Equation",equList[[$i]]/.Unevaluated->Defer},dom[[1]]],{$i,Length[equList]}],dom,Prolog->{},Evaluate@FilterRules[{plotOptions},Options@spdPlot],PlotStyle->Dashed,Evaluate@spdAltPlotLabel["Derivative",Length[equList]]];
Return[result,spdPlotDerivative]
}]
};


(*Inverse plot. Thanks Dulvino*)
spdPlotInverse[equ_,dom:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotOptions:OptionsPattern[]]:={
Module[{
(*Turn equations into a standard table form that can be used*)
equList=Flatten@{equ},
result
},{
(*
Print[equList];
Print@Flatten[Evaluate@Table[spdEquPlotGInverse[equList[[$i]],Column[{"Inverse",Normal@spdInverse[equList[[$i]],dom[[1]]]}],"Original Equation",dom[[1]]],{$i,Length[equList]}],4];
Print@ParametricPlot[Evaluate@Table[spdEquPlotGInverse[equList[[$i]],Column[{"Inverse",Normal@spdInverse[equList[[$i]],dom[[1]]]}],"Original Equation",dom[[1]]],{$i,Length[equList]}],dom,Prolog->{},Evaluate@FilterRules[{plotOptions},Options@spdPlot],PlotStyle->Dashed,Evaluate@spdAltPlotLabel["Inverse",Length[equList]]];
*)
result=ParametricPlot[Evaluate@Table[spdEquPlotGInverse[equList[[$i]],Column[{"Inverse",Normal@spdInverse[equList[[$i]],dom[[1]]]}],"Original Equation",dom[[1]]],{$i,Length[equList]}],dom,Prolog->{},Evaluate@FilterRules[{plotOptions},Options@spdPlot],PlotStyle->Dashed,Evaluate@spdAltPlotLabel["Inverse",Length[equList]]];
(*result=ParametricPlot[Evaluate@Table[Evaluate@spdEquPlotGInverse[equList[[$i]],Column[{"Inverse",Normal@spdInverse[equList[[$i]],dom[[1]]]}],"Original Equation",dom[[1]]],{$i,Length[equList]}],dom,Prolog->{},Evaluate@FilterRules[{plotOptions},Options@spdPlot],PlotStyle->Dashed,Evaluate@spdAltPlotLabel["Inverse",Length[equList]]];*)
Return[result,spdPlotInverse]
}]
};


(*Find the automatic plotrange of any equation*)
spdPlotInitRange[equ_,dom:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotOptions:OptionsPattern[]]:={
Module[{
initRange=PlotRange/.plotOptions},{
If[AllTrue[Flatten@{initRange},NumericQ],{
(*If PlotRange is a single number, make it not a single number*)
	If[Length[Flatten@{initRange}]==1,initRange=Flatten@{initRange,-initRange}]}
,{
initRange = (PlotRange/.Options[Plot[Flatten@{equ},dom,PlotRange->spdPlotRange]])[[2]]
}];
Return[{Floor@Min[initRange],Ceiling@Max[initRange]},spdPlotInitRange]
}]
};


(*Plot all of this
This is a new way of declaring domain and range that $i'm not use to. From Joseph's GigaChadPlot of course :)*)
spdPlot[equ_:x,dom:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotOptions:OptionsPattern[]]:={

(*Plz stay quiet, thanks
Immuatable variables
Sometimes you have to delete DynamicModule for some unknown reason
Do not make this Quiet*)
DynamicModule[{
equList=Hold@equ,
(*Dragging area*)
initPos,finalPos,
(*Range to plot over*)
idomLow=dom[[2]],idomHigh=dom[[3]],domLow=dom[[2]],domHigh=dom[[3]],
rangeLow,rangeHigh,irangeLow,irangeHigh,
(*The left input bar variables*)
findXInput=0, findXBool=True, findXG={{}},
findYInput=0, findYBool=True, findYG={{}},
findGradInput=0, findGradBool=True, findGradG={{}},
findTangentInput=1, findTangentBool=False, findTangentG={{}},
findNormalInput=1, findNormalBool=False, findNormalG={{}},
asyBool=False, asyV={{}}, asyO={{}}, asyG={{}},
poiBool=True, poiG={{}},
epBool=True, epG={{}},
holeBool=True, holeG={{}},
inverseBool=False, inverseG={{}},
derivativeBool=False, derivativeG={{}},
integrateBool=False, integrateG={{}}
},
{
(*Unevaluate all equations*)
If[Head@equ===List,
equList=Evaluate@Table[Extract[equList,{1,i},Unevaluated],{i,Length[equ]}],
equList={Unevaluated@equ}];

(*Desect any user global functions in equList*)
equList=spdDesectFunction@equList;

(*Parse domain*)
If[dom[[2]]!=-10&&dom[[3]]!=10,{
idomLow=domLow=dom[[2]],
idomHigh=domHigh=dom[[3]]
}];


(*Round to whole numbers to allow fractions*)
{rangeLow,rangeHigh}=spdPlotInitRange[equ,{dom[[1]],domLow,domHigh},plotOptions];
irangeLow = rangeLow;
irangeHigh = rangeHigh;

(*Calculate variables as needed
Calculate the 6 variables below first because they seem more important*)
findXG=Dynamic[
If[findXBool,
spdPointsG[Complement[spdFindX[equList,findXInput,dom[[1]],rangeLow,rangeHigh],{{}}],"Find X",spdColourPoint2]
],
SynchronousUpdating->False,
TrackedSymbols:>{findXInput,findXBool,rangeLow,rangeHigh}];

findYG=Dynamic[
If[findYBool,
spdPointsG[Complement[spdFindY[equList,findYInput,dom[[1]],domLow,domHigh],{{}}],"Find Y",spdColourPoint2]
],
SynchronousUpdating->False,
TrackedSymbols:>{findYInput,findYBool,domLow,domHigh}];

findGradG=Dynamic[
If[findGradBool,
spdPointsG[Complement[spdFindGrad[equList,findGradInput,dom[[1]],domLow,domHigh,rangeLow,rangeHigh],{{}}],"Find Gradient",spdColourPoint1]
],
SynchronousUpdating->False,
TrackedSymbols:>{findGradInput,findGradBool,domLow,domHigh,rangeLow,rangeHigh}];

findTangentG=Dynamic[
If[findTangentBool,{
(*Find the line and then the point, so that the point is on top*)
spdLinesG[Complement[spdFindTangent[equList,findTangentInput,dom[[1]]],{{}}],spdColourLine],
spdPointsG[Complement[spdFindX[equList,findTangentInput,dom[[1]],rangeLow,rangeHigh],{{}}],"Find Tangent",spdColourLine]
}
],
SynchronousUpdating->False,
TrackedSymbols:>{findTangentInput,findTangentBool,rangeLow,rangeHigh}];

findNormalG=Dynamic[
If[findNormalBool==True,{
(*Find the line and then the point, so that the point is on top*)
spdLinesG[Complement[spdFindNormal[equList,findNormalInput,dom[[1]]],{{}}],spdColourLine],
spdPointsG[Complement[spdFindX[equList,findNormalInput,dom[[1]],rangeLow,rangeHigh],{{}}],"Find Normal",spdColourLine]
}
],
SynchronousUpdating->False,
TrackedSymbols:>{findNormalInput,findNormalBool,rangeLow,rangeHigh}];

epG=
Dynamic[
	If[epBool,
		spdPointsG[spdEndPoints[equList,dom[[1]],Rationalize@domLow,Rationalize@domHigh,Rationalize@rangeLow,Rationalize@rangeHigh],"End Point",spdColourPoint3]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{epBool,domLow,domHigh,rangeLow,rangeHigh}
];

poiG=
Dynamic[
	If[poiBool,
		spdPointsGPOI[spdPOI[Flatten@{equ},dom[[1]],domLow,domHigh,rangeLow,rangeHigh],"Point of Intersection",spdColourPoint1]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{poiBool,domLow,domHigh,rangeLow,rangeHigh}
];
asyV=
Dynamic[
	If[asyBool,
		spdAsyV[equList,dom[[1]],domLow,domHigh]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{asyBool,domLow,domHigh}
];
asyO=
Dynamic[
	If[asyBool,
		spdAsyO[equList,dom[[1]]]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{asyBool}
];

asyG={
(*Vertical Asymptote Lines*)
Dynamic[
	If[asyBool,
		spdLinesG[asyV[[1]],spdColourLine]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{asyV}
],
(*Vertical Asymptote Points*)
Dynamic[
	If[asyBool,
		spdPointsG[
			Complement[
				(*Find all points over all equations*)
				Flatten[Map[spdFindX[equList,#,dom[[1]],rangeLow,rangeHigh]&,spdAsyParser[asyV[[1]]]],1],
			{{}}
			],
		"Vertical Asymptote",
		spdColourLine
		]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{asyV}
],
(*Other Asymptotes*)
Dynamic[
	If[asyBool,
		spdPlotFaster[asyO[[1]],{dom[[1]],domLow,domHigh},PlotRange->{rangeLow,rangeHigh},PlotStyle->Dashed]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{asyO}
],
(*Other Asymptote Points of Intersection*)
Dynamic[
	If[asyBool,
		spdPointsGPOI[
			(*Find all asymptote and intersection points of intersection*)
			Flatten[
				(*Iterate over all equations*)
				Map[
					(*Find points of insection*)
					spdPOI[
						Flatten@{
							(*Parse data out of asymptotes*)
							Map[{#[[1]][[1]][[1]][[1]][[1]]}&,
								(*Remove all empty points*)
								Complement[
									spdAsyO[equList,dom[[1]]],
								{{}}]
							],
						#},
					dom[[1]],domLow,domHigh,rangeLow,rangeHigh]&,
				equList],
			1],
		"Other Asymptote",
		spdColourLine
		]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{asyO}
]
};
(*Print@Map[{#[[1]][[1]][[1]][[1]]}&,
								(*Remove all empty points*)
								Complement[
									spdAsyO[equList,dom[[1]]],
								{{}}]
							];
							
Print@
					(*Find points of insection*)
					spdPOI[
						Flatten@{
							(*Parse data out of asymptotes*)
							Map[{#[[1]][[1]][[1]][[1]][[1]]}&,
								(*Remove all empty points*)
								Complement[
									spdAsyO[equList,dom[[1]]],
								{{}}]
							],
						equList[[1]]},
					dom[[1]],domLow,domHigh,rangeLow,rangeHigh];*)

(*Print@spdPlotInverse[equList,{dom[[1]],rangeLow,rangeHigh},PlotRange->{domLow,domHigh},plotOptions];
*)
inverseG=
Dynamic[
	If[inverseBool,
		spdPlotInverse[equList,{dom[[1]],rangeLow,rangeHigh},PlotRange->{domLow,domHigh},plotOptions]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{inverseBool,domLow,domHigh,rangeLow,rangeHigh}
];
derivativeG=
Dynamic[
	If[derivativeBool,
		spdPlotDerivative[equList,{dom[[1]],domLow,domHigh},PlotRange->{rangeLow,rangeHigh},plotOptions]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{derivativeBool,domLow,domHigh,rangeLow,rangeHigh}
];
integrateG=
Dynamic[
	If[integrateBool,
		spdPlotIntegrate[equList,{dom[[1]],domLow,domHigh},PlotRange->{rangeLow,rangeHigh},plotOptions]
	],
	SynchronousUpdating->False,
	TrackedSymbols:>{integrateBool,domLow,domHigh,rangeLow,rangeHigh}
];
holeG=
Dynamic[
	If[holeBool,
		spdPointsG[
			Complement[
				spdHoles[equList,dom[[1]]],
			{{}}
			],
			"Hole",
			spdColourPoint4
		]
	],
	
	SynchronousUpdating->False,
	TrackedSymbols:>{holeBool,domLow,domHigh,rangeLow,rangeHigh}
];


(*Core plotting function*)
spdStyle@TableForm[{
(*Pseudo Manipulate area
Map to make all lists rows*)
Row@{TableForm[Map[If[Head[#]===List,Row@#,#]&,{
(*Dynamic@holeG,
spdHoles[equList,{dom[[1]],domLow,domHigh},PlotRange->{rangeLow,rangeHigh},plotOptions],
*)"Domain",
{spdInputField[Dynamic[domLow,SynchronousUpdating->False],Tiny],
spdInputField[Dynamic[domHigh,SynchronousUpdating->False],Tiny]},
"Range",
{spdInputField[Dynamic[rangeLow,SynchronousUpdating->False],Tiny],
spdInputField[Dynamic[rangeHigh,SynchronousUpdating->False],Tiny]},
{Checkbox[Dynamic[findXBool,SynchronousUpdating->False]],"Find X"},
spdInputField[Dynamic[findXInput,SynchronousUpdating->False],Small],
{Checkbox[Dynamic[findYBool,SynchronousUpdating->False]],"Find Y"},
spdInputField[Dynamic[findYInput,SynchronousUpdating->False],Small],
{Checkbox[Dynamic[findGradBool,SynchronousUpdating->False]],"Find Gradient"},
spdInputField[Dynamic[findGradInput,SynchronousUpdating->False],Small],
{Checkbox[Dynamic[findTangentBool,SynchronousUpdating->False]],"Find Tangent at X"},
spdInputField[Dynamic[findTangentInput,SynchronousUpdating->False],Small],
{Checkbox[Dynamic[findNormalBool,SynchronousUpdating->False]],"Find Normal at X"},
spdInputField[Dynamic[findNormalInput,SynchronousUpdating->False],Small],
(*Checkbox for points of intersection*)
{Checkbox[Dynamic[poiBool,SynchronousUpdating->False]],"Points of Intersect"},
{Checkbox[Dynamic[epBool,SynchronousUpdating->False]],"End Points"},
{Checkbox[Dynamic[holeBool,SynchronousUpdating->False]],"Holes"},
(*Checkbox for toggling asymptotes*)
{Checkbox[Dynamic[asyBool,SynchronousUpdating->False]],"Asymptotes"},
{Checkbox[Dynamic[inverseBool,SynchronousUpdating->False]],"Inverse"},
{Checkbox[Dynamic[derivativeBool,SynchronousUpdating->False]],"Derivative"},
{Checkbox[Dynamic[integrateBool,SynchronousUpdating->False]],"Integrate"}
}], TableSpacing->{0, 0}
],
EventHandler[
Dynamic[
Show[
spdPlotFast[
	equList,
	{dom[[1]],domLow,domHigh},
	Evaluate@FilterRules[{PlotRange->{rangeLow,rangeHigh},PlotRangeClipping->False,PlotLegends->(equList/.Unevaluated->Defer),plotOptions},Options@Plot]
	][[1]],

(*Inverse and derivative plots. These can be behind everything*)
		If[inverseBool,inverseG[[1]],{}],
		If[derivativeBool,derivativeG[[1]],{}],
		If[integrateBool,integrateG[[1]],{}],
		
(*Plot asymptotes after that, since they are long*)
		If[asyBool,asyG[[3]][[1]],{}],
		If[asyBool,{asyG[[1]][[1]],asyG[[2]][[1]],asyG[[4]][[1]]},{}],
(*Find these points on the graph before poi and cp, since Find is less important*)
		If[epBool,epG[[1]],{}],
		If[findTangentBool,findTangentG[[1]],{}],
		If[findNormalBool,findNormalG[[1]],{}],
		If[findXBool,findXG[[1]],{}],
		If[findYBool,findYG[[1]],{}],
		If[findGradBool,findGradG[[1]],{}],
(*Plot points next, since they are small*)
		If[poiBool,poiG[[1]],{}],
		If[holeBool,holeG[[1]],{}]
	],
(*Weird quirk to make ] better
Dynamic Options
Plz don't process anything here*)
	None,
(*These are symbols to track. Don't keep track of anything else*)
	TrackedSymbols:>{domLow,domHigh,rangeLow,rangeHigh,findXInput,findXBool,findXG,findYInput,findYBool,findYG,findGradInput,findGradBool,findTangentInput,findGradG,findTangentInput,findTangentBool,findTangentG,findNormalInput,findNormalBool,findNormalG,asyBool,asyG,poiBool,poiG,epBool,epG,holeBool,holeG,inverseBool,inverseG,derivativeBool,derivativeG,integrateBool,integrateG},
	(*THE GOLDEN PIECE, this only run once*)
	CachedValue->Once@ToBoxes[spdPlotFast[Rationalize@equ,dom,{PlotRange->{rangeLow,rangeHigh},plotOptions}]],
	SynchronousUpdating->False
	],{"MouseDown":>({initPos=MousePosition[],idomLow=domLow,idomHigh=domHigh,irangeLow=rangeLow,irangeHigh=rangeHigh}),"MouseDragged":>({finalPos=MousePosition[],domLow=spdDragSens(idomHigh-idomLow)(initPos[[1]]-finalPos[[1]])+idomLow,domHigh=spdDragSens(idomHigh-idomLow)(initPos[[1]]-finalPos[[1]])+idomHigh,rangeLow=spdDragSens(irangeHigh-irangeLow)(-initPos[[2]]+finalPos[[2]])+irangeLow,rangeHigh=spdDragSens(irangeHigh-irangeLow)(-initPos[[2]]+finalPos[[2]])+irangeHigh})}
]
	}},
(*Tableform options*)
	TableDirections->Column,TableSpacing->{0, 0}]
}[[1]],SaveDefinitions->True]
}[[1]];


spdPlotCore[equ_:x,dom:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotOptions:OptionsPattern[]]:={
DynamicModule[{
equList=Hold@equ,
(*These {{}} are 100% necessary*)
asyV={{}},asyVP={{}},
asyO={{}},asyOP={{}},
endpoints={{}},
findX={{}},
findY={{}},
findGrad={{}},
poi={{}},
holes={{}},
(*Dragging variables*)
initPos,finalPos,idomLow=dom[[2]],idomHigh=dom[[3]],domLow=dom[[2]],domHigh=dom[[3]],irangeLow,irangeHigh,rangeLow,rangeHigh
},{

If[Head@equ===List,
equList=Evaluate@Table[Extract[equList,{1,i},Unevaluated],{i,Length[equ]}],
equList={Unevaluated@equ}
];

(*Desect any user global functions in equList*)
equList=spdDesectFunction@equList;

(*Parse domain*)
If[dom[[2]]!=-10&&dom[[3]]!=10,{
idomLow=domLow=dom[[2]],
idomHigh=domHigh=dom[[3]]
}];


(*Round to whole numbers to allow fractions*)
{rangeLow,rangeHigh}=spdPlotInitRange[equ,{dom[[1]],domLow,domHigh},plotOptions];
irangeLow = rangeLow;
irangeHigh = rangeHigh;


(*Asymptotes*)
asyV=Dynamic[spdLinesG[spdAsyV[equList,dom[[1]],domLow,domHigh],spdColourLine],SynchronousUpdating->False,TrackedSymbols:>{domHigh}];
asyO=Dynamic[spdPlotFaster[spdAsyO[equList,dom[[1]]],{dom[[1]],domLow,domHigh},PlotRange->{rangeLow,rangeHigh},PlotStyle->Dashed],SynchronousUpdating->False,TrackedSymbols:>{domHigh}];
asyVP=Dynamic[spdPointsG[Complement[Flatten[Map[spdFindX[equList,#,dom[[1]],rangeLow,rangeHigh]&,spdAsyParser@spdAsyV[equList,dom[[1]],domLow,domHigh]],1],{{}}],"Vertical Asymptote",spdColourLine],SynchronousUpdating->False,TrackedSymbols:>{asyV,domHigh}];
asyOP=Dynamic[spdPointsGPOI[Flatten[Map[spdPOI[Flatten@{Map[{#[[1]][[1]][[1]][[1]][[1]]}&,Complement[spdAsyO[equList,dom[[1]]],{{}}]],#},dom[[1]],domLow,domHigh,rangeLow,rangeHigh]&,equList],1],"Other Asymptote",spdColourLine],SynchronousUpdating->False,TrackedSymbols:>{asyO,domHigh}];
(*Keeping all points and lines dynamic keeps spdPlotCore small*)
endpoints=Dynamic[spdPointsG[spdEndPoints[equList,dom[[1]],Rationalize@domLow,Rationalize@domHigh,Rationalize@rangeLow,Rationalize@rangeHigh],"End Point",spdColourPoint3],SynchronousUpdating->False,TrackedSymbols:>{domHigh}];
holes=Dynamic[spdPointsG[Complement[spdHoles[equList,dom[[1]]],{{}}],"Hole",spdColourPoint4],SynchronousUpdating->False,TrackedSymbols:>{equList}];
findX=Dynamic[spdPointsG[Complement[spdFindX[equList,0,dom[[1]],rangeLow,rangeHigh],{{}}],"Y Intercept",spdColourPoint2],SynchronousUpdating->False,TrackedSymbols:>{rangeHigh}];
findY=Dynamic[spdPointsG[Complement[spdFindY[equList,0,dom[[1]],domLow,domHigh],{{}}],"X Intercept",spdColourPoint2],SynchronousUpdating->False,TrackedSymbols:>{domHigh}];
(*Make gradient slightly more resilient*)
findGrad=Dynamic[spdPointsG[Complement[spdFindGrad[equList,0,dom[[1]],domLow,domHigh,rangeLow,rangeHigh],{{}}],"Turning Point",spdColourPoint1],SynchronousUpdating->False,TrackedSymbols:>{domHigh}];
poi=Dynamic[spdPointsGPOI[spdPOI[equList,dom[[1]],domLow,domHigh,rangeLow,rangeHigh],"Point of Intersection",spdColourPoint1],SynchronousUpdating->False,TrackedSymbols:>{domHigh}];

EventHandler[
Dynamic[Show[
	spdPlotFast[
		equList,
		{dom[[1]],domLow,domHigh},
		Evaluate@FilterRules[{PlotRange->{rangeLow,rangeHigh},PlotRangeClipping->False,PlotLegends->(equList/.Unevaluated->Defer),plotOptions},Options@Plot]
	][[1]],
	asyV[[1]],
	asyO[[1]],
	asyVP[[1]],
	asyOP[[1]],
	endpoints[[1]],
	holes[[1]],
	findX[[1]],
	findY[[1]],
	findGrad[[1]],
	poi[[1]]
	],
	None,
	SynchronousUpdating->False,
	TrackedSymbols:>{asyO,asyV,asyOP,asyVP,endpoints,findX,findY,findGrad,poi,domHigh},
	(*THE GOLDEN PIECE, this only run once*)
	CachedValue->Once@ToBoxes[spdPlotFaster[equ,dom,{PlotRange->{rangeLow,rangeHigh},plotOptions}]]
	],{"MouseDown":>({initPos=MousePosition[],idomLow=domLow,idomHigh=domHigh,irangeLow=rangeLow,irangeHigh=rangeHigh}),"MouseDragged":>({finalPos=MousePosition[],domLow=spdDragSens(idomHigh-idomLow)(initPos[[1]]-finalPos[[1]])+idomLow,domHigh=spdDragSens(idomHigh-idomLow)(initPos[[1]]-finalPos[[1]])+idomHigh,rangeLow=spdDragSens(irangeHigh-irangeLow)(-initPos[[2]]+finalPos[[2]])+irangeLow,rangeHigh=spdDragSens(irangeHigh-irangeLow)(-initPos[[2]]+finalPos[[2]])+irangeHigh})}
](*Must not return*)
}[[1]],SaveDefinitions->True
]}[[1]];


spdQuestionDerivative[equList_,dom_:x,range_:Range[20],length_:50,name_:"Derivatives Speed Run"]:={
DynamicModule[{
randomEqu,
equListRandom,
equListRandomHidden,
symbols,
solution,
showAll=False,
longestAnswer
},{
(*Iterate over all lists*)
equListRandom=
(*Repeat for length*)
	Table[
			{
			randomEqu=RandomChoice@equList;
			(*Parse out all non global symbols, and dom from random equList*)
			symbols=DeleteDuplicates@Select[Level[randomEqu,{0,\[Infinity]}],Head@#===Symbol&&Context@#=!="System`"&&#=!=dom&];
			i,
			(*Sub random values from range into symbols of equation*)
			solution=randomEqu/.Map[#->RandomChoice[range]&,symbols],
			(*Find derivative*)
			Evaluate@D[Extract[solution,1],dom]
			},
	{i,length}
	]/.Unevaluated->Defer;

(*Get list of equations without answers*)
equListRandomHidden=Evaluate@Table[{i,equListRandom[[i]][[2]],"               "},{i,length}];

(*UI*)
Column@{
	Row@{
	name,Checkbox[Dynamic[showAll,SynchronousUpdating->False]]
	},
	Dynamic[If[showAll,Grid@equListRandom,Grid@equListRandomHidden],SynchronousUpdating->False,TrackedSymbols:>{showAll}]
}

(*Return[equListRandom,spdQuestionDerivative]*)
}[[1]],SaveDefinitions->True
]
}[[1]]


spdQuestionDerivatives[equ_,dom_:x,range_:Range[20],length_:45,row_:5,name_:"Derivates Speed Run"]:={
Module[{equList=Hold@equ},{
(*Unevaluate all equations*)
If[Head@equ===List,
equList=Evaluate@Table[Extract[equList,{1,i},Unevaluated],{i,Length[equ]}],
equList={Unevaluated@equ}];
Return[
Row@Table[spdQuestionDerivative[equList,dom,range,length,name],row],
spdQuestionDerivatives]
}
]
}


(*Unprotect these letters which are protected for some reason*)
Unprotect[Evaluate@Select[Names["spd`*"],StringLength@#==1&]];


(*Initialise resource function asymptote*)
ResourceFunction["Asymptotes"];


(* ::Text:: *)
(*Functions:*)
(*Find any X point on graph			Done*)
(*Find any Y point on graph			Done*)
(*Find points of intersection			Done*)
(*Find critical points					Done*)
(*Find asymptotes					Done*)
(*Compute points with tooltip 			Done*)
(*Compute asymptotes with tooltip		Done*)
(*Plot all of this*)
(*Show a list of graphics and delete last printed cell*)
(**)
(*Goals:*)
(*ParallelSubmit everything to emulate async computing. This should be faster.*)
(*Keep everything in modules.*)
(*spdPlot will be the master function where: equList, Dispatch rules and everything else that is constant will be computed firsts*)
(**)
(*Things to note:*)
(*spdPlot is the only function that can take expressions not in a flattened list*)
(**)
(*v12 Feedback*)
(*Slow*)
(*Manipulate looks too clunky*)
(**)
(*Changes for v13.2*)
(*Fix continuously calculations	Done*)
(*Rewrite manipulate				Done*)
(*Keep track of previous location for xPoint and yPoint		Done?*)
(**)
(*Changes for v13.3*)
(*Change spdPlot modules to dynamic module to improve compatibility with manipulate*)
(**)
(*Goals for v14*)
(*Set global variables for Font & Colours		Done*)
(*Write a spdShow function for dynamically showing FindX and FindY*)
(*Failed. $i changed DynamicModule to Module.*)
(*$i fixed something and now DynamicModule works?*)
(*Print points and asymptotes if you press on it	Done*)
(**)
(*Goals for v14.1*)
(*Change every Module to DynamicModule, so that everything is saved over sessions.	Done*)
(*Rationalise equation list of spdPlot, so that every solution is in a slightly nicer form.	Done*)
(*Maintain evaluations, even after session has closed.	Done*)
(*Support all plot rules.	Done*)
(*Replace lines with infinite lines.	Done*)
(*Remove spdPlot range and AxesLabel, as they are unnecessary now.	Done*)
(**)
(*Goals for v14.2*)
(*Return of async (ParallelSubmit & WaitAll).	Done*)
(*Incremental printing.	Done*)
(*Disable incremental printing for Manipulate.*)
(**)
(*v14.7 reflection*)
(*Async is still slow (duh) so $i reverted to sync parallel.*)
(*Changed Manipulate to Incremental because Mathematica doesn't like it when you steal names of built-in functions. At least it works as expected now.*)
(**)
(*Goals for v16.1*)
(*Keep track of equation for each point	Done*)
(*Give each point a gradient	Done*)
(*Fix the timeout issue when processing takes too long	Done*)
(*Set everything to sync, and made the dynamic inside plot to be async*)
(**)
(*Goals for v16.2*)
(*Make incremental better	Done*)
(**)
(*Goals for v16.3*)
(*Open spdPlot in another Mathematica FrontEnd		Done?*)
(*You can open a new frontend now*)
(*Increase DynamicEvaluationTimeout to infinity	Done?*)
(*It is 10 billion years, so yeah it may as well be infinity*)
(**)
(*Goals for v17*)
(*FindAQuadratic	Done*)
(*FindACubic	Done*)
(*spdLinesG	Done*)
(**)
(*Goals for v17.1*)
(*Find Normal	Done*)
(*Plot Inverse	Done*)
(*Label every plot equation	Done?*)
(*Incomplete, but close enough so $i'm not fixing it*)
(*Memorize everything*)
(*Problem: Memoization doesn't work with DynamicModule*)
(*Fix: Just memoize the final value.*)
(**)
(*Goals for v17.1.3*)
(*Add derivatives	Done*)
(*Add integrals		Done*)
(*Add a way to change the default message	Done*)
(*Allocate better names for everything	Done*)
(*Write this script through a shortener (spdShortener)	Done*)
(*Set all unsaved variables.	Done*)
(**)
(*Goals for v17.1.4*)
(*Remove incremental flag	Done*)
(*Update documentation for spdPlot	Done*)
(*Run this file on boot	Done*)
(**)
(*Goals for v18*)
(*Add images to plot		Done*)
(*Update documentation again	Done*)
(*Make it so that other plot functions don't respond to Prolog	Done*)
(*spdImage function should scale images with the dimensions of the graph 	Failed, but the alternative is satisfactory*)
(**)
(*Goals for v18.4*)
(*Make all plots actually interactive		Done*)
(*Give tangent and normal a point of intersection		Failed, can't be bothered incorporating it*)
(*Fix weird bug that doesn't allow printed symbols to be used in equations	Done, it works now!*)
(*Improve plotting memoization	Done, not spdPlot uses less than half the storage, especially with more equations.*)
(**)
(*Goals for v18.5*)
(*Save tooltipString	Done? $i don't know why this works but it does*)
(*Update - Removed DynamicModule in favour of With*)
(**)
(*Goals for v18.8*)
(*Dynamically edit the equation, domain and range	Done*)
(**)
(*Goals for v19*)
(*Find any linear, log and circular graph*)
(*After finding these equations, hover over them to preview spdPlotFast graph	Done*)
(*Make a plot function that is core math friendly. spdPlotCore	Done*)
(*Change initPlot to initRange, cause $i only need the range	Done*)
(*Release hold from spdEquPlotG so that it can be used in calculations	Done*)
(*Update dynamic domain and range to affect inverse, derivative and integral graphs	Done*)
(*Function that finds the equation for your formula and points	WIP, fix log and sin graphs*)
(*Update find gradient	Done*)
(*Make spdPlot persistent again	Done*)
(*Reduce spdPlotCore size	Failed, idk why*)
(*Enable lists in spdPlot initialisation again	Done*)
(*Remove memoization from graphics cause that isn't a long computation	Done*)
(*Stop computations within out of the range	Done*)
(*Solve for x and y rather than getting x and subbing in y	Done*)
(*Replace With with Module	Done*)
(**)
(*Goals for v19.1*)
(*rangeHigh needs its range increased by .1 for some reason. Idk why	Done*)
(*Make spdPlot fit every single plotting function	Done*)
(*Select the second term of spdPOI	Done?*)
(*Use more amperands in spdFindGrad	Done*)
(*Simplify gradients of each point to make it less intimidating	Done*)
(*Adding .1 to rangeHigh fixes something (?)	Done*)
(**)
(*Goals for v19.2*)
(*Make range automatic again. Do not default to {-10,10}.	Done*)
(*Scrollable plot function?		Failed, can't be bothered making it work*)
(*Only calculate things that do not have their checkboxes ticked?	Done*)
(*Place block in all small variables where variables may be overwritten	WIP, probably implement later if $i am still interested*)
(*Add PlotRange to user customization options	Done*)
(*Simplify all equations in points	Done*)
(*TextRecognise to turn images into equations	Failed, this was an ambitious idea anyways*)
(*Speech to Text?	Failed, obviously*)
(*Fix point of intersection finder for good.	Done*)
(**)
(*Goals for v19.3*)
(*Document every spdPlot function*)
(*Make spdFindGrad find Y as well	Done*)
(*Fix compatibility with manipulate	Done. Remove Incremental flag as it is unnecessary now*)
(*Make spdPlot options the same as Plot	Done*)
(*Remove domain low and high from horizontal asymptote finding	Done*)
(**)
(*Goals for v19.4*)
(*Fix gradient and point of intersection decimal instead of fraction issue. Also remove bad .1 code	Done*)
(**)
(*Goals for v19.5*)
(*Update Horizontal Asymptote (spdAsyV) to only use a single way to find asymptotes	Done*)
(*Give spdEquPlotG a second message that is below the expression, instead of only above*)
(*Modify all plot functions to show the function domain, range and original equations	Done? Need to fix domain and range for inverse, derivative and integral graphs*)
(**)
(*Goals for v19.6*)
(*Add labels to all not original plots*)
(*Remove grey box for initialisation*)
(**)
(*Goals for v19.7*)
(*Do 19.6	DONE THANK YOU JAY YOUR CHOCOLATE IS MAGIC FOOD*)
(*Swap domain and range for spdInverse so that it is plotted correctly	Done*)
(*Fix root approximation checker for point of intersection	Done*)
(*Allow inverse plot to skip inverse	Done*)
(*{x,2 x^2,200-x,x Sin[x]}*)
(*Sometimes FunctionRange or FunctionDomain can not be calculated, so allow failures if it can't be calculated. Check spdEquPlotG and horizontal asymptotes.*)
(*Done. Function is called spdErrorChecker. All expressions passed to spdErrorChecker must be Unevaluated.*)
(*Make spdEquPlotG dynamic.	Failed, this did absolutely nothing*)
(*Correct alternate plot domain and range, again.	Done*)
(*Stop recalculating findX, findY, points of intersection and gradients every time equation changes to maybe improve manipulate.	Failed, they aren't tracked in the first place*)
(**)
(*Notes for v19.7*)
(*In Manipulate plot, you must set the TrackedSymbols. For example*)
(*Manipulate[spdPlot[(x^2-2)/(a x)],{a,-10,10}]*)
(*Wants to recalculate itself every frame. Manipulate[spdPlot[(x^2-2)/(a x)],{a,-10,10},TrackedSymbols:>{a}]*)
(*Is good. Only calculates when a is moved.*)
(**)
(*Goals for v19.8*)
(*Revert to the old vertical asymptote finding	Done*)
(*Make spdPlotFast dynamic to make it look faster	Done*)
(*Make a spdStyle function and apply it to all strings	Done*)
(**)
(*Goals for v19.9*)
(*Turn everything into pure functions that doesn't use optional arguments	Done*)
(*Turn root approximations into normal Mathematica syntax so that its less difficult to read	Done*)
(*Add TrackedSymbol to spdPlotFast so that it stops wasting cpu cycles recalculating something it already has	Done*)
(*Remove alternate plot label curly brackets	Done*)
(*Make point of intersection left to right		Done*)
(**)
(*Goals for v19.9.1*)
(*Replace with with module	Done*)
(*Corrected spdspdStyle to spdStyle in spdCopyThis	Done, actually $i removed memoization from all non-compute intensive tasks*)
(*Fix this case: {4 x,(-2+x^2)/(3 x),x^2} with poi	Done, now every point of intersection is converted to radicals*)
(*Deprecate DynamicEvaluationTimeout setter	Done*)
(**)
(*Notes for v19.9.1*)
(*This version saves 0.1 kb per equation :/*)
(**)
(*Goals for v20*)
(*Make a draggable plot function	Done, idk why it works. It just does*)
(**)
(*Goals for v20.1*)
(*Sync up domain and range with dragging in spdPlot	Done*)
(*Make spdPlotCore calculate itself once		Done*)
(*Make dragging of plot function async	Done, although may remove later cause its not smooth anymore*)
(**)
(*Goals for v20.2*)
(*Remove queued method for EventHandling		Done*)
(*Compile equation for scrolling	Done*)
(*Make a function for finding the initial range of any plotting function	Done, make sure it is a flat list for AllTrue to check*)
(**)
(*Goals for v20.3*)
(*Implement a separate draggable plot into core and main	Done*)
(*Fixed lines, now points are broken because core range does not move	Done*)
(*Idk how $i made it work. It just does. $i actually don't understand why. Its so brain*)
(*dead and fragile spaghetti that is 100% necessary and will not work in any other way*)
(*Match dragging sensitivity with the size of the plot. So if you drag half the domain, the graph should translate half the domain	Done*)
(*Fix this case: spdPlotCore[{x,3}]	Done, $i finally fixed horizontal asymptote finding. Only took 17 versions :D*)
(*Make sure domainLow, domainHigh, rangeLow and rangeHigh are not machine numbers in spdPlot and spdPlotCore by applying floor and ceiling to all of them	Done*)
(*Remove domain restrictions from finding tangents and normals	Done*)
(*Save Definitions in spdPlotDraggable	Done*)
(**)
(*Notes for v20.3*)
(*Manipulate doesn't save without SaveDefinitions->True*)
(**)
(*Goals for v20.4*)
(*Radicalise roots so that they can be graphed.*)
(*4.75t^4-31.3t^3+69t^2-61.7t can not be graphed*)
(*Solution: Always convert plotting points into numeric form before being turned into graphics. They are close enough to their true positions.*)
(*Done. Now $i can remove all instances of ToRadical. And points will always be plotted.*)
(*Fixed CachedValues, so they are only used once on startup and don't create memory leaks anymore	when put in manipulate		Done*)
(*Made spdPlotFaster as an alternative to spdPlotFast, without anything Dynamic*)
(*Rationalise all pointsG and linesG tooltipstrings	Done*)
(*Add a point to findTangent and findNormal	Done*)
(**)
(*Goals for v20.6*)
(*Remove memoization from spdPlotEpicycle cause it just takes up too much memory	Done*)
(*Implement the draggable version of spdPlotDraggable from version 20.0, because that was the fastest version of spdPlot.	Failed, wayyy too hard to implement*)
(*Tangent and normal calculations may fail when out domain and range	Done, now they still work*)
(*Remove slant asymptote calculations that are the same as the original equation	Done*)
(*Calculate end points. Do this using FunctionDomain and FunctionRange. Find all LessEqual and GreaterEqual heads.	Failed, can't be bothered making it work yet.*)
(**)
(*Notes for v20.6*)
(*Almost nothing changed. Maybe $i am reaching the limit of my capabilities.*)
(**)
(*Goals for v20.7*)
(*Make preset rules with Dispatch to reduce copy pasting	Done*)
(*Find end points	DOne*)
(*Replace Floor@x+1 with Floor[x+1] for the expected rounding. Do that with Ceiling as well		Failed, does absolutely nothing*)
(*Map[#->&,{list of variables}] should be applied to every dispatch rule	Done*)
(*Give inputfield a Hold command again		Done*)
(*Modified slant asymptote calculations to maybe fix them for once?	Done, they are still broken*)
(**)
(*Goals for v20.8*)
(*Format all of it so that its somewhat readable	Will do in next version*)
(*Memoize everything being processed, and make iteratable stuff listable.	Done*)
(**)
(*Goals for v20.9*)
(*Make eventhandler for all draggable plots universal	Done*)
(*Collect all variables used in all plot functions into a single maintainable list Done*)
(*Format all of it so that its somewhat readable*)
(*$i give up with slant asymptotes. $i will use mathematica's built in implementation of finding slant asymptotes	Done! Works so much better*)
(**)
(*Goals for v20.10*)
(*Don't make EventHandler universal, cause it breaks everything	Done*)
(**)
(*Goals for v20.11*)
(*Complete transition to private functions	Done*)
(*Simplify points and lines rounded and complete form	Done*)
(*Give better colours for spdPlot with canva colour wheel	Done*)
(**)
(*Thanks Alp for laying giving useful UI advice*)
(*Goals for v20.12*)
(*1 button for all asymptotes	Done*)
(*Fix spdPlotCore	Done*)
(*Colour code point of intersection finder	Done*)
(*Fix persistence		Failed, I deleted it instead*)
(**)
(*Goals for v20.13*)
(*Plot points for horizontal and vertical asymptotes		Done*)
(*Make small functions to replace Setting Options Failed, made other small functions for parsing horizontal and vertical asymptote points	Done*)
(*Fix alternate plot labels	Done*)
(**)
(*Goals for v20.14*)
(*Make print colourful	Done*)
(*Fix some bugs with horizontal and vertical asymptotes not printing correctly	Done*)
(*Include PolynomialQuotient into other asymptote finding again*)
(*GreaterEqual and LessEqual shouldn't be included as asymptote	Failed, removing that broken code now*)
(**)
(*Goals for v20.15*)
(*Remove horizontal asymptote finding, and replace vertical asymptote finding	Done VertAsympFind[func_]:=Solve[1/(func//Simplify)==0,Reals]*)
(*Fix memoization again for protected functions (all of them)	Done*)
(*Fix very weird bug where Plot refuses to appear with list in asyG??????	Done*)
(*Implement holes	Extremely difficult*)
(*Speed Holes can not be held. It will always automatically evaluate*)
(**)
(*Goals for v20.16*)
(*Fix auto evaluating thingo*)
(*Remove result from every function because it is counter productive	Failed cause it wasn't even the problem*)
(*WHY DOES POINTS OF INTERSECTION JUST WORK INDEPENDENTLY AND CORRECTLY*)
(*Use unevaluated for processing, defer for displaying	Done*)
(*Add lazy evaluation to everything in speed plot	Done*)
(*Simplify calculation of end points and holes	Done*)
(*Normalize inverse calculation to remove conditional statements	Done*)
(*Support spdPlotCore, again	Done*)
(*Implement a standardized way of memoization support for all calculating functions that do any computing	Done*)
(*Make a separate version with SaveDefinitions*)
(**)
(*Goals for v20.17*)
(*Fix case spdPlot[1/x^2] and spdPlotCore[1/x^2]	Done*)
(*Unevaluate every function during memoization*)
(*D[\!\( *)
(*TagBox["x",*)
(*HoldForm]\)^2/(2 Sqrt[1+\!\(\**)
(*TagBox["x",*)
(*HoldForm]\)])+2 \!\(\**)
(*TagBox["x",*)
(*HoldForm]\) Sqrt[1+\!\(\**)
(*TagBox["x",*)
(*HoldForm]\)],x]*)
(*-((\!\( *)
(*TagBox["x",*)
(*HoldForm]\)^2 Derivative[1][HoldForm][x])/(4 (1+\!\(\**)
(*TagBox["x",*)
(*HoldForm]\))^(3/2)))+(2 \!\(\**)
(*TagBox["x",*)
(*HoldForm]\) Derivative[1][HoldForm][x])/Sqrt[1+\!\(\**)
(*TagBox["x",*)
(*HoldForm]\)]+2 Sqrt[1+\!\(\**)
(*TagBox["x",*)
(*HoldForm]\)] Derivative[1][HoldForm][x]*)
(**)
(*Fixe5:*)
(*Match spdEquPlotGInverse with spdEquPlotG	Done*)
(*spdAsyO has broken itself in a very terrible way. It gives TerminatedEvaluation[RecursionLimit]. The most useless error message	Done, turns out I just needed to remove one of the evaluates*)
(*Fix inverse plot	Done. Turns out I just had to retype the entire thing*)
(*Simplify derivatives and integrals after calculating them for speed plot	Done*)
(*Fix asymptote point of intersection finding. [[1]][[1]][[1]] was not enough, we needed [[1]][[1]][[1]][[1]][[1]]	Done*)
(*Fix spdPlotCore asymptote as well*)
(**)
(*Fixe6:*)
(*Do not unevaluate extracted values for spdEquPlotG	Done*)
(*Convert user made global functions into their not global function counter parts	Done*)
(*Fix vertical asymptote plotting	Done, now all vertical asymptotes being inputted must be held or unevaluated and in a list*)
(**)
(*Fixe7:*)
(*Fix clearing all spd global symbols	Done*)
(*Push spdFormula to main code	Done*)
(**)
(*Fixe8:*)
(*Patch composite functions eg f(g(x))*)
(**)
(*Goals for v21.0*)
(*Make a function to make derivative questions	Done*)
(*Clear everything in the global space properly		Done*)
(*Finally implement a persistent version			Done*)
(*Fix protected letters	Done*)
