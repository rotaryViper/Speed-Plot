(* ::Package:: *)

(*Entire Asymptotes.wl source code*)
Needs["NumericalCalculus`"];


BeginPackage["ResourceFunctionHelpers`", {"ResourceFunctionHelpers`CommonFunctions`", "NumericalCalculus`"}];


Unprotect[Asymptotes];
ClearAll[Asymptotes];
Asymptotes::usage= "Compute the asymptotes to a plane curves";


Begin["`Asymptotes`Private`"];


Asymptotes::argn = "Asymptotes must be called with 3 or 4 arguments.";
(* Adding work around for default option setting in WANE *)
Asymptotes[args___] /; Quiet[CurrentValue[EvaluationNotebook[], StyleDefinitions] === "WolframAlphaNotebook.nb"] :=
	Block[{$Assumptions = True, res},
	res = iAsymptotes[args];
	If[!(3<=Length[{args}]<=4),
		RFHMessage[Asymptotes::argn]
	];
	res /; !MatchQ[res, _iAsymptotes]
]

Asymptotes[args___] := Block[
	{res = iAsymptotes[args]},
	If[!(3<=Length[{args}]<=4),
		RFHMessage[Asymptotes::argn]
	];
	res /; !MatchQ[res, _iAsymptotes]
]


iAsymptotes[expr:Equal[lhs_, rhs_], indepVar_Symbol, depVar_Symbol] /;
	!(Head[lhs] === Symbol && Length[VarList[{lhs, rhs}]] == 1) && !MatchQ[lhs, _?usersymbolQ[_]] && !MatchQ[lhs, depVar] := Module[
	{
		asym, display, asymFnX, asymFnY, asymSameQFunc,
		horiz, vert, parab, oblique, trueParab, other
	},
	asymFnX = asymptotesOfImplicitEquations[expr, {depVar, indepVar}] /. $Failed -> {};  (* y[x] = <fn of x> *)

	asymFnY = Reverse[asymptotesOfImplicitEquations[expr, {indepVar, depVar}] /. $Failed -> {}, 4];

	asymSameQFunc = Quiet[TrueQ[#1 === #2 || Eliminate[{#1, #2}, depVar] || #1 === Reverse[#2, 2]]]&;
	horiz = (DeleteDuplicates[Join[asymFnX[[1]], asymFnY[[2]]], asymSameQFunc] // combinePlusMinus // DeleteDuplicates // First[#, {}]&);
	vert = (DeleteDuplicates[Join[asymFnX[[2]], asymFnY[[1]]], asymSameQFunc] // combinePlusMinus // DeleteDuplicates // First[#, {}]&);
	parab = (DeleteDuplicates[Join[asymFnX[[3]], asymFnY[[3]]], asymSameQFunc] // combinePlusMinus // DeleteDuplicates // First[#, {}]&);

	oblique= Cases[parab /. $Failed -> {}, Alternatives[
		{depVar -> _?(PolynomialQ[#,indepVar]&&Exponent[#,indepVar]===1&) | PlusMinus[_?(PolynomialQ[#,indepVar]&&Exponent[#,indepVar]===1&)], indepVar -> _},
		{indepVar -> _?(PolynomialQ[#,depVar]&&Exponent[#,depVar]===1&)| PlusMinus[_?(PolynomialQ[#,depVar]&&Exponent[#,depVar]===1&)], depVar -> _}
	]];
	trueParab = Cases[parab /. $Failed -> {}, Alternatives[
		{depVar -> _?(PolynomialQ[#,indepVar]&&Exponent[#,indepVar]===2&), indepVar -> _},
		{indepVar -> _?(PolynomialQ[#,depVar]&&Exponent[#,depVar]===2&), depVar -> _}
	]];
	other= Complement[parab /. $Failed -> {}, Join[oblique, trueParab]];

	asym= <|"Horizontal" -> horiz, "Vertical" -> vert, "Oblique" -> oblique, "Parabolic" -> trueParab, "Other"-> other|> // DeleteCases[{}]
]


iAsymptotes[HoldPattern[Equal][depVar_?usersymbolQ, expr_], indepVar_?usersymbolQ, depVar_?usersymbolQ] := Block[{res = iAsymptotes[expr, indepVar, depVar]},
	res /; FreeQ[res, iAsymptotes | $Failed]
]

iAsymptotes[expr_, indepVar_?usersymbolQ, depVar_?usersymbolQ] /; FreeQ[expr, _Equal] := Module[
	{horiz, vert, pmHoriz, pmVert, parabolic, pmParabolic, res, oblique, trueParab, other},

	horiz = horizontalAsymptotes[expr, indepVar, depVar];
	pmHoriz = combinePlusMinus[horiz];

	vert = verticalAsymptotes[expr, indepVar, depVar];
	pmVert = combinePlusMinus[vert];

	parabolic = parabolicAsymptotes[expr, indepVar, depVar];
	pmParabolic = combinePlusMinus[parabolic];

	oblique= Cases[pmParabolic /. $Failed -> {}, Alternatives[
		{depVar -> _?(PolynomialQ[#,indepVar]&&Exponent[#,indepVar]===1&), indepVar -> _},
		{indepVar -> _?(PolynomialQ[#,depVar]&&Exponent[#,depVar]===1&), depVar -> _}
	]];
	trueParab = Cases[pmParabolic /. $Failed -> {}, Alternatives[
		{depVar -> _?(PolynomialQ[#,indepVar]&&Exponent[#,indepVar]===2&), indepVar -> _},
		{indepVar -> _?(PolynomialQ[#,depVar]&&Exponent[#,depVar]===2&), depVar -> _}
	]];
	other= Complement[pmParabolic /. $Failed -> {}, Join[oblique, trueParab, horiz /. $Failed -> {}, vert /. $Failed -> {}]];

	res= <|"Horizontal" -> pmHoriz, "Vertical" -> pmVert, "Oblique" -> oblique, "Parabolic" -> trueParab, "Other"-> other|>;
	res = DeleteCases[res, $Failed];
	res = With[{expnd = Expand[expr], simp = Simplify[expr]}, Select[#, FreeQ[#, expr | expnd | simp]&]& /@ res];
	res= DeleteCases[res, {}];

	res
]


iAsymptotes[expr_, indepVar_Symbol, depVar_Symbol, All]:= Flatten[Values[iAsymptotes[expr, indepVar, depVar]], 1]
iAsymptotes[expr_, indepVar_Symbol, depVar_Symbol, type_String]:= First[Values[KeySelect[iAsymptotes[expr, indepVar, depVar], #===type&]], {}]


selectBestForm[a_, b : Times[-1, _]] := a

selectBestForm[a : Times[-1, _], b_] := b

selectBestForm[a_, b_] := If[LeafCount[a] < LeafCount[b], a, b]

combinePlusMinus[$Failed]:= $Failed
combinePlusMinus[l_List] := Quiet[l //. {
		{a___, {as_, x_ -> la_}, b___, {as_, x_ -> lb_}, c___} :> {a, b, c, {as, Rule[x, PlusMinus[Abs[lb]]]}} /; (PossibleZeroQ[la + lb] || (la === Infinity && lb === -Infinity) || (la === -Infinity && lb === Infinity)),
		{a___, {y_ -> rLim_, x_ -> limPt_}, b___, {y_-> lLim_, x_ -> limPt_}, c___} :> {a, b, c, {y -> PlusMinus[rLim], Rule[x, limPt]}} /; (PossibleZeroQ[rLim + lLim] || (rLim === Infinity && lLim === -Infinity)),
		{a___, {y_ -> rLim_, x_ -> limPt_}, b___, {y_ -> lLim_, x_ -> limPt_}, c___} :> {a, b, c, {y -> PlusMinus[lLim], Rule[x, limPt]}} /; (PossibleZeroQ[rLim + lLim] || (rLim === -Infinity && lLim === Infinity))
}]


horizontalAsymptotes[expr_, x_, y_] := Block[{right, left, asym},
  	asym = Flatten[Reap[
    	left = Limit[expr, x -> -Infinity];
     	If[FreeQ[left, DirectedInfinity] && NumericQ[left] && Element[left, Reals],
      		Sow[{left, x -> -Infinity}]
      	];
     	right = Limit[expr, x -> Infinity];
     	If[FreeQ[right, DirectedInfinity] && NumericQ[right] && Element[right, Reals],
      		Sow[{right, x -> Infinity}]
      	]
	][[-1]], 1];
	If[asym === {}, Return[$Failed]];
	asym = MapAt[y->#&, asym, {All, 1}]
]


processInfinities[expr_] := Block[{lims},
	lims = Select[expr, FreeQ[#, Complex]&];
	Which[
		lims === {},
			Return[{}],
		MatchQ[lims, {Infinity} | {-Infinity}],
			First @ lims,
		True,
			(lims /. C[_] -> 1) //. infinityRules
	]
]

infinityRules = {
					{-Infinity, Infinity} -> PlusMinus[Infinity],
					{Infinity, -Infinity} -> PlusMinus[Infinity],
					{Infinity, Infinity} -> Infinity,
					{-Infinity, -Infinity} -> -Infinity,
					{a_, b_} /; MatchQ[a, Infinity] || MatchQ[b, Infinity] :> Infinity,
					{a_, b_} /; MatchQ[a, -Infinity] || MatchQ[b, -Infinity] :> -Infinity
				};


checkVerticalAsymptotes[expr_, x_ -> p_] :=
	RFHContainsQ[{
		Assuming[Element[C[1], Integers], Quiet@Limit[expr, x -> p, Direction -> -1]],
		Assuming[Element[C[1], Integers], Quiet@Limit[expr, x -> p, Direction -> 1]]
		}, DirectedInfinity]

verticalAsymptotes[expr_, x_, y_] := Block[{pts, otherPoints, res},
  	pts = Quiet @ Reduce[1/expr == 0, x, Reals];
  	otherPoints = Quiet @ DeleteCases[Flatten[{Reduce[# == 0, x], Reduce[1/#==0, x]}& /@ Level[expr, {0, Infinity}]], False];
      If[pts === $Aborted && otherPoints === {}, Return[$Failed]];
  	pts = Cases[{pts, otherPoints}, _?usersymbolQ == _, {0, Infinity}];
  	pts = DeleteDuplicates[Rule @@@ pts];
  	pts = Cases[pts, pt_ /; checkVerticalAsymptotes[expr, pt]];
  	If[pts === {}, Return[$Failed]];
  	res = {
  		processInfinities[{
  			Assuming[Element[C[1], Integers], Limit[expr, #, Direction -> -1]],
  			Assuming[Element[C[1], Integers], Limit[expr, #, Direction -> 1]]
  		}], #} & /@ pts;
  	res = DeleteCases[res, l_ /; ListQ[First[l]] || RFHContainsQ[l, _Complex]];
  	If[res === {}, Return[$Failed]];
  	MapAt[y->#&, res, {All, 1}]
]


univariateRationalQ[Power[expr_, _Integer]] := univariateRationalQ[expr]
univariateRationalQ[expr_Plus] := VectorQ[List @@ expr, univariateRationalQ]
univariateRationalQ[expr_Times] := VectorQ[List @@ expr, univariateRationalQ]
univariateRationalQ[expr_] := With[{vars = VarList[expr]}, Length[vars] <= 1 && PolynomialQ[expr, vars]]
univariateRationalQ[n_] /; NumericQ[n] = True;
univariateRationalQ[__] = False;


Clear[parabolicAsymptotes];
parabolicAsymptotes[expr_, x_, y_] /; univariateRationalQ[expr] := Block[{rat, num, den},
  	rat = Together[expr];
  	num = Numerator[rat];
  	den = Denominator[rat];

  	If[(Exponent[num, x] - Exponent[den, x] >= 1) && !MatchQ[expr, PolynomialQuotient[num, den, x]],
  		{{y-> PolynomialQuotient[num, den, x], x->PlusMinus[Infinity]}},
   		$Failed
   	]
]

parabolicAsymptotes[c_. (a_. x_^n_Integer + k_.)^m_., x_, y_, opts: OptionsPattern[]] /; n == 1/m && FreeQ[{a, k, c}, x] && Im[a] == 0 := If[
	(* if n is even *)
	EvenQ[n],
	(* asymptotes in both directions are the same *)
	{{y -> c Sqrt[a] Abs[x], x-> PlusMinus[Infinity]}},
	(* else direction of asymtote depends on sign of a *)
	If[ a > 0,
		{{y -> c Sqrt[a] x, x-> Infinity}},
		{{y -> -c Sqrt[-a] x, x-> -Infinity}}
	]
]

parabolicAsymptotes[c_. (x_^n_ + k_.)^m_., x_, y_, opts: OptionsPattern[]] /; FreeQ[{k, c}, x] := {{y -> c x^(m n), x->PlusMinus[Infinity]}}

parabolicAsymptotes[Sqrt[a_. x_^2 + b_. ] + c_. x_, x_, y_, opts: OptionsPattern[]] /; FreeQ[{a, b, c}, x] && c < 0 && Sqrt[a] == -c :=
	{{y -> -2 Sqrt[a] x, x -> Infinity}}

parabolicAsymptotes[expr_, x_, y_, opts: OptionsPattern[]] /; !PolynomialQ[expr, x] := Block[
	{
		leftAsym, rightAsym,
		series, rat, poly, limit, nlimit
	},
    If[FreeQ[expr, x], Return[$Failed]];
	(* find right asymptote (x\[Rule] +Infinity*)
  	series = Quiet @ Series[ToRadicals[expr], {x, Infinity, 1}];
  	rightAsym = If[FreeQ[series, HoldPattern @ SeriesData[_, _, {__?NumericQ}, ___]],
   		$Failed,
   		rat = rat = Together[Normal[
   			series //. {
   				Times[f_, s_SeriesData] :> Normal[s, f]
   			}
   		]];
   		If[PolynomialQ[Numerator[rat], x] && PolynomialQ[Denominator[rat], x],
    		poly = Factor @ Simplify @ PolynomialQuotient[Numerator[rat], Denominator[rat], x];
    		limit = {Limit[poly - ToRadicals[expr], x -> Infinity], Limit[poly - ToRadicals[expr], x -> -Infinity]} /. Interval[__] :> 0;
    		If[RFHContainsQ[limit, 0],
    			poly,
 				nlimit = Quiet @ Chop[#, 10^-3] & /@ {NLimit[poly - expr, x -> Infinity], NLimit[poly - expr, x -> -Infinity]};
 				If[RFHContainsQ[limit, 0],
 					poly,
 					$Failed
 				]
 			],
    		$Failed
    	]
   	];

   (* find left asymptote (x\[Rule] -Infinity*)
  	series = Quiet @ Series[ToRadicals[expr], {x, -Infinity, 1}];
  	leftAsym = If[FreeQ[series, HoldPattern @ SeriesData[_, _, {__?NumericQ}, ___]],
   		$Failed,
   		rat = Together[Normal[
   			series //. {
   				Times[f_, s_SeriesData] :> Normal[s, f]
   			}
   		]];

   		If[PolynomialQ[Numerator[rat], x] && PolynomialQ[Denominator[rat], x],
    		poly = Factor @ Simplify @ PolynomialQuotient[Numerator[rat], Denominator[rat], x];

    		limit = {Limit[poly - ToRadicals[expr], x -> Infinity], Limit[poly - ToRadicals[expr], x -> -Infinity]} /. Interval[__] :> 0;
    		If[RFHContainsQ[limit, 0],
    			poly,
 				nlimit = Quiet @ Chop[#, 10^-3] & /@ {NLimit[poly - expr, x -> Infinity], NLimit[poly - expr, x -> -Infinity]};
 				If[RFHContainsQ[limit, 0],
 					poly,
 					$Failed
 				]
 			],
    		$Failed
    	]
  	];
  	{
  		If[leftAsym === $Failed, Nothing, {y->leftAsym, x->-Infinity}],
  		If[rightAsym === $Failed, Nothing, {y->rightAsym, x->Infinity}]
  	} /. {} -> $Failed
]


Clear[asymptotesOfImplicitEquations];
asymptotesOfImplicitEquations[cx_. x_^2 + cy_. y_^2 == k_, {x_, y_} | {y_, x_}] /; (cx cy < 0 && (NumericQ[k] || (usersymbolQ[k] && FreeQ[{x,y},k]))) :=
	{
		{}, (* no horizontal asymptotes *)
		{}, (* no vertical asymptotes *)
		{{{y -> Sqrt[Abs[cx]/Abs[cy]] x, x-> PlusMinus[Infinity]}, {y -> -Sqrt[Abs[cx]/Abs[cy]] x, x-> PlusMinus[Infinity]}}} (*2 oblique asymptotes*)
	}

asymptotesOfImplicitEquations[eqn_Equal, {x_, y_}] := Module[
	{
		soln, fnsOfx, asymptotes, res,
		parabolic = {}, vertical = {}, horizontal = {}
	},
  	soln = Reduce[eqn, {x, y}, Reals];

  	If[MatchQ[soln, _Reduce | $Aborted], Return[$Failed]];
  	fnsOfx = DeleteDuplicates @ Cases[soln, e_Equal /; RFHContainsQ[Last[e], x] :> Last[e], {0, Infinity}];

  	If[fnsOfx == {}, Return @ $Failed];

    (* try to find parabolic asymptotes *)
    asymptotes = parabolicAsymptotes[#, x, y] & /@ fnsOfx;
    asymptotes = DeleteDuplicates[DeleteCases[asymptotes, $Failed]];
    parabolic = Select[asymptotes, FreeQ[N[#], Complex]&];

	(* try to find vertical asymptotes *)
	asymptotes = verticalAsymptotes[#, x, y] & /@ fnsOfx;
	asymptotes = DeleteCases[asymptotes, $Failed];
	vertical = Select[asymptotes, FreeQ[N[#], Complex]&];

	(* try to find horizontal asymptotes *)
	asymptotes = horizontalAsymptotes[#, x, y] & /@ fnsOfx;
	asymptotes = DeleteCases[asymptotes, $Failed];
	horizontal = Select[asymptotes, FreeQ[N[#], Complex]&];

	res = {horizontal, vertical, parabolic}
]


End[];


EndPackage[];


(*Speed Plot below*)


BeginPackage["spd`"];
Unprotect @@ Names["spd`*"];
ClearAll @@ Names["spd`*"];
ClearAll @@ Names["spd`Private`*"];
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

This is find edges and ";spdHoles::usage="Find holes in expressions
spdHoles[ equ_ , dom_:x ]";spdPlotFast::usage="Foundation plot function for spdPlot
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
To find answers";spdDesmos::usage="Recreation of Desmos, with 15 equations.
It calculates:
X & Y intercepts
Turning Points
Points of intersection
End points";spdQuestionDerivatives::usage="Make a list of fast derivative questions.
spdQuestionDerivatives[ equ_ , dom_:x , range_:Range[20] , length_:45 , row_:4 , name_: ''Derivates Speed Run'' ]

The default length and row are perfect for making the most out of each a4 piece of paper

Enter a list of expressions into equ_
(Optional)Enter a symbol into dom_ to use as the domain
(Optional)Enter a list of numbers into range_ to sub into the symbols that aren't domain in the list of expressions
(Optional)Enter a number into length_ for each run
(Optional)Enter a number into row_ for the number of runs
(Optional)Enter a string into name_ to name runs";


ClearAll @@ Evaluate @ Select[Names["*"], StringContainsQ[#, "spd"]&];

(*Copy equation*)

spdCopyThis[equ_, message_ : spdStyle["Copy This"]] :=
    Button[message, CopyToClipboard[equ]];

(*Tells you an error*)

spdErrorChecker = Quiet @ Check[#, spdStyle @ "Error"]&;

(*-------------------------Quality of life functions-------------------------*)

(*Execute a new mathematica process*)

spdNewNoteBook :=
    SystemOpen[FileNameJoin[{$InstallationDirectory, "Mathematica"}]];

(*Convert equation into clean TraditionalForm*)

spdForm[expr_] :=
    StandardForm @ TraditionalForm @ expr;

(*-------------------------Global constants-------------------------
Customise these to your heart's content.
Restart mathematica when changing these options, or they many not apply.
*)

spdFont = "Nunito";

spdColourPlot = Automatic;

spdColourPoint1 = Red;

spdColourPoint2 = Green;

spdColourPoint3 = Blue;

spdColourPoint4 = Black;

spdColourLine = Lighter @ Red;

spdRoundPrecision = MachinePrecision;

spdMessage = "Sharing is Caring";

spdPlotSize = Medium;

spdPlotRange = Automatic; (*Just like a persistent PlotRange*)

spdDragSens = .002;

spdPlotFaster[equ_, dom : {_, _?NumericQ, _?NumericQ} : {Global`x, -10, 10},
     plotOptions : OptionsPattern[]] :=
    Plot[Evaluate @ Flatten @ {equ}, dom, Evaluate @ FilterRules[{plotOptions
        }, Options @ Plot], Background -> spdColourPlot, AspectRatio -> 1, PlotLegends
         -> "Expressions", ImageSize -> spdPlotSize, PlotLabel -> spdStyle[spdMessage
        ]];


(*Function for complete the square*)

spdCompleteTheSquare[a_,b_,c_,Optional[verbose_,False]]:={
If[verbose==True,
(*Declare complete the square string first*)
Module[{
cmpString={"Step 1: \!\(\*SuperscriptBox[\(ax\), \(2\)]\) + bx + c = 0",
HoldForm[Global`x^2 + b Global`x + c == 0],
"",
"Step 2: \!\(\*SuperscriptBox[\(x\), \(2\)]\) + \!\(\*FractionBox[\(b\), \(a\)]\)x + \!\(\*FractionBox[\(c\), \(a\)]\) = \!\(\*FractionBox[\(0\), \(a\)]\)",
HoldForm[Global`x^2 + b/a Global`x + c/a == 0],
"",
"Step 3: \!\(\*SuperscriptBox[\(x\), \(2\)]\) + \!\(\*FractionBox[\(b\), \(a\)]\)x = -\!\(\*FractionBox[\(c\), \(a\)]\)",
HoldForm[Global`x^2 + b/a Global`x == -(c/a)],
"",
"Step 4: \!\(\*SuperscriptBox[\(x\), \(2\)]\) + \!\(\*FractionBox[\(b\), \(a\)]\)x + (\!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\) = -\!\(\*FractionBox[\(c\), \(a\)]\) + (\!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\)",
HoldForm[Global`x^2 + b/a Global`x + (b/(2a))^2 == -(c/a) + (b/(2a))^2],
"",
"Step 5: (x + \!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\) = -\!\(\*FractionBox[\(c\), \(a\)]\) + (\!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\)",
HoldForm[(Global`x + b/(2a))^2 == -(c/a) + (b/(2a))^2],
"",
"Step 6: a(x + \!\(\*FractionBox[\(b\), \(2  a\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\) + c - \!\(\*FractionBox[SuperscriptBox[\(b\), \(2\)], \(4  a\)]\) = 0"}}
,{
(*Create Table of Styled string, that iterates over complete the square spring*)
Print[TableForm@Table[spdStyle[spdForm@cmpString[[$i]]],{$i,Length[cmpString]}]]
}]];
a (Global`x+b/(2a))^2+c-b^2/(4a)==0//spdForm
}[[1]];


Begin["`Private`"];


(*-------------------------StartUp quality of life things-------------------------*)

(*Functions to make listable*)

SetAttributes[{spdFindX, spdFindY, spdFindGrad, spdFindTangent, spdFindNormal,
     spdEndPoints, spdHoles, spdStyle, spdDesectFunction, spdEquPlotG, spdEquPlotGInverse,
     spdInverse, spdAsyO}, Listable];

SetAttributes[{spdPlot, spdQuestionDerivatives}, HoldAll
    ];

(*Protect certain values for spdPlot*)

(*
spdPlotVariables={equList,equInput,initPos,finalPos,idomLow,idomHigh,domLow,domHigh,irangeLow,irangeHigh,rangeLow,rangeHigh,findXInput,findXBool,findXG,findYInput,findYBool,findYG,findGradInput,findGradBool,findTangentInput,findGradG,findTangentInput,findTangentBool,findTangentG,findNormalInput,findNormalBool,findNormalG,asyBool,asyG,poiBool,poiG,epBool,epG,inverseBool,inverseG,derivativeBool,derivativeG,integrateBool,integrateG};
*)

(*-------------------------Small functions-------------------------*)

spdAltPlotLabel =
    PlotLabels ->
        Placed[
            (*If there is more than one expression, use a list. Otherwise
                 do not use a list*)If[#2 > 1,
                Table[spdStyle @ #, #2]
                ,
                spdStyle @ #
            ]
            ,
            {{Scaled[.9], Before}}
        ]&;

(*Placed[Table[spdStyle@"Derivative",5],{{Scaled[.9],Before}}]*)

spdInputField = InputField[#1, Evaluate @ FilterRules[{FrameMargins ->
     None, ContentPadding -> False, Alignment -> {Left, Center}, FieldSize
     -> #2}, Options @ InputField]]&;

spdAsyParser = Map[#[[1]][[2]][[2]]&, Evaluate @ #]&;

spdDesectFunction[expr_] :=
    {
        Module[
            {
                (*Check head of expression*)context = Context @ Evaluate
                     @ Head @ Unevaluated @ expr, head = Evaluate @ Head @ Unevaluated @ 
                    expr
            }
            ,
            (*If head of expression is in the global context*)
            If[context === "",
                Return[Extract[DownValues[Evaluate @ OwnValues[head][[
                    1]][[2]]], {1, 2}, Unevaluated], spdDesectFunction]
                ,
                Return[Unevaluated @ Unevaluated @ expr, spdDesectFunction
                    ]
            ]
        ]
    }


(*Inverse any equation*)

spdInverse[expr_, dom_:Global`x] :=
    {
        Module[{result = spdErrorChecker @ Unevaluated @ TableForm[Simplify[
            y /. Solve[dom == (expr /. dom -> y), y, Reals]]]},
            Unprotect[spdInverse];
            spdInverse[Unevaluated @ expr, dom] = result;
            Protect[spdInverse];
            Return[result, spdInverse]
        ]
    };


(*Reverse gradient for findX normal*)

spdInverseGradient[expr_] :=
    With[{grad = expr[[2]][[1]][[3]]},
        ReplacePart[expr, {2, 1, 3} -> -(1 / grad)]
    ]


(*Find any X point on graph*)

spdFindX[equ_, xCoord_, dom_:Global`x, rangeLow_ : -\[Infinity], rangeHigh_:\[Infinity]] :=
    {
        Quiet @
            Module[{yCoord, result = {}},
                {
   (*Compute yCoord, and check if yCoord is even on the graph
			Select removes complex numbers*)yCoord = Select[Flatten @ Cases[Quiet
     @ {equ /. dom -> xCoord}, _?NumericQ], FreeQ[#, _Complex] && Floor @
     rangeLow <= # <= Ceiling @ rangeHigh&];
                    (*Return something if yCoord is not empty*)
                    If[yCoord == {},
                        {}
                        ,
   (*Return all xCoords with yCoords. Delete duplicates
			Include gradient
			Caches results for spdPlot*)
                        result = Simplify @ {Defer @ equ, DeleteDuplicates
                             @ Table[{xCoord, yCoord[[$i]], D[equ, dom] /. dom -> xCoord}, {$i, Length[
                            yCoord]}]}
                    ]
                    ,
                    Unprotect[spdFindX];
                    spdFindX[Unevaluated @ equ, xCoord, dom, rangeLow,
                         rangeHigh] = result;
                    Protect[spdFindX];
                    Return[result, spdFindX]
                }
            ]
    };


(*Find any Y point on graph*)

spdFindY[equ_, yCoord_, dom_:Global`x, domLow_ : -10, domHigh_:10] :=
    {
        (*Find any point on the graph*)Quiet @
            Module[{
                (*Compute xCoord, and check if xCoord is even on the 
                    graph*)xCoord = Flatten @ Cases[dom /. Solve[equ == yCoord && Floor @
                     domLow <= dom <= Ceiling @ domHigh, dom, Reals], _?NumericQ], result
                     = {}
            },
                {
                    (*Return something if xCoord is not empty*)If[xCoord
                         == {},
                        {}
                        ,
   (*Return all xCoords with yCoords. Delete duplicates
				Caches results for spdPlot*)
                        result = {Defer @ equ, Simplify @ DeleteDuplicates
                             @ Table[{xCoord[[$i]], yCoord, D[equ, dom] /. dom -> xCoord[[$i]]}, 
                            {$i, Length[xCoord]}]};
                    ];
                    Unprotect[spdFindY];
                    spdFindY[Unevaluated @ equ, yCoord, dom, domLow, 
                        domHigh] = result;
                    Protect[spdFindY];
                    Return[result, spdFindY]
                }
            ]
    };


(*Find gradients*)

spdFindGrad[equ_, gradient_, dom_:Global`x, domLow_ : -10, domHigh_:10, rangeLow_
     : -\[Infinity], rangeHigh_:\[Infinity]] :=
    {
        Quiet @
            Module[{cp, result, y},
                {
                    (*Iterate over all lists*)cp = Select[{dom, y} /.
                         Solve[D[equ, dom] == gradient && equ == y && Floor @ domLow <= dom <=
                         Ceiling @ domHigh && Floor @ rangeLow <= y <= Ceiling @ rangeHigh, {
                        dom, y}, Reals], NumericQ[#[[1]]]&];
   (*Output cp X and Y, along with the equation
			Caches results for spdPlot*)
                    result = Simplify @ {Defer @ equ, Table[Flatten @
                         {cp[[$i]], gradient}, {$i, Length[cp]}]};
                    Unprotect[spdFindGrad];
                    spdFindGrad[Unevaluated @ equ, gradient, dom, domLow,
                         domHigh, rangeLow, rangeHigh] = result;
                    Protect[spdFindGrad];
                    Return[result, spdFindGrad]
                }
            ]
    };


(*Find points of intersection*)

spdPOI[equList_, dom_:Global`x, domLow_ : -10, domHigh_:10, rangeLow_ : -\[Infinity], 
    rangeHigh_:\[Infinity]] :=
    {
        (*Find any point on the graph*)Quiet @
            Module[{poi, result, y, equListC = Table[Extract[Hold[equList
                ], {1, i}, Defer], {i, Length[equList]}]},
                {
                    (*Make sure equList is longer than 1*)If[Length[equList
                        ] > 1,
   (*Calculate the intersection
				Caches results for spdPlot*)
                        result =
                            DeleteDuplicates @
                                Flatten[
                                    (*Parallel Tables caused too much
                                         trouble*)Table[
                                        (*Plz stay silent, even in parallel
                                             processing*)Table[
                                            {
                                                (*Remove lines that don
                                                    't intersect*)poi = Select[{dom, y} /. Solve[equList[[equ1]] == equList
                                                    [[equ2]] == y && Floor @ domLow <= dom <= Ceiling @ domHigh && Floor 
                                                    @ rangeLow <= y <= Ceiling @ rangeHigh, {dom, y}, Reals], NumericQ[#[[
                                                    1]]]&];
                                                (*Some numbers are too
                                                     big for plotting. Make them numeric if they are too big*)
                                                (*Include equations*)
                                                    
                                                Simplify @
                                                    {
                                                        {equListC[[equ1
                                                            ]], equListC[[equ2]]}
                                                        ,
                                                        (*Include points
                                                            *)
                                                        Table[
                                                            {
                                                                poi[[
                                                                    $i]][[1]]
                                                                ,
                                                                poi[[
                                                                    $i]][[2]]
                                                                ,
                                                                (*Include
                                                                     gradients*)
                                                                {(D[equList
                                                                    [[equ1]], dom]) /. dom -> poi[[$i]][[1]], (D[equList[[equ2]], dom]) /.
                                                                     dom -> poi[[$i]][[1]]}
                                                            }
                                                            ,
                                                            {$i, Length[
                                                                poi]}
                                                        ]
                                                    }
                                            }
                                            ,
                                            {equ2, equ1 + 1, Length[equList
                                                ]}
                                        ]
                                        ,
                                        {equ1, Length[equList]}
                                    ]
                                    ,
                                    2
                                ]
                        ,
                        (*If equList has a length of 1 or less, return
                             nothing*)
                        result = {}
                    ];
                    Unprotect[spdPOI];
                    spdPOI[Unevaluated @ equList, dom, domLow, domHigh,
                         rangeLow, rangeHigh] = result;
                    Protect[spdPOI];
                    Return[result, spdPOI]
                }
            ]
    };


(*Find vertical asymptotes*)

spdAsyV[equList_, dom_:Global`x, domLow_ : -10, domHigh_:10] :=
    {
        Quiet @
            Module[{lines, result},
                {
                    (*Remove all non numbers*)lines = Select[Flatten 
                        @ Table[dom /. Solve[1 / equList[[$i]][[1]] == 0 && Floor @ domLow <=
                         dom <= Ceiling @ domHigh], {$i, Length[equList]}], NumericQ];
   (*Convert into useable points
			Caches results for spdPlot*)
                    result =
                        (*TooltipString*)
                        Table[
                            {
                                {"Vertical Asymptote", {N[lines[[$i]],
                                     spdRoundPrecision], lines[[$i]]}}
                                ,
                                (*Points*)
                                {{lines[[$i]], 0}, {lines[[$i]], 1}}
                            }
                            ,
                            {$i, Length[lines]}
                        ];
                    Unprotect[spdAsyV];
                    spdAsyV[equList, dom, domLow, domHigh] = result;
                    Protect[spdAsyV];
                    Return[result, spdAsyV]
                }
            ]
    };


(*Find other asymptotes*)

spdAsyO[equ_, dom_:Global`x] :=
    {
        Module[{result},
            {
                result =
                    spd`Private`spdEquPlotG[
                        (*Remove all the wrong asymptotes, non-asymptotes
                            , duplicates*)DeleteCases[
                            Complement[
                                DeleteDuplicates @
                                    Flatten @
                                        {
                                            (*Just find the other asymptotes
                                                *)Map[$Noty /. #&, Flatten @ Map[ResourceFunctionHelpers`Asymptotes[Unevaluated
                                                 @ equ, dom, $Noty, #]&, {"Oblique", "Parabolic", "Other"}]]
                                            ,
                                            (*Thank you for staying quiet
                                                *)
                                            Quiet @ PolynomialQuotient[
                                                Numerator[Together @ equ], Denominator[Together @ equ], dom]
                                        }
                                ,
                                {$Noty, equ}
                            ]
                            ,
                            _PolynomialQuotient
                        ]
                        ,
                        "Asymptote"
                        ,
                        Column @ {"Original Equation", Defer @ equ}
                        ,
                        dom
                    ];
                Unprotect[spdAsyO];
                spdAsyO[Unevaluated @ equ, dom] = result;
                Protect[spdAsyO];
                Return[result, spdAsyO]
            }
        ]
    };


(*Find tangent*)

spdFindTangent[equ_, xCoord_, dom_:Global`x] :=
    {
        Quiet @
            Module[{grad, yCoord, tangent, result},
                {
                    (*Find gradient*)grad = D[equ, dom] /. dom -> xCoord
                        ;
                    (*Find yCoord*)
                    yCoord = equ /. dom -> xCoord;
                    (*Find tangent*)
                    tangent = grad (dom - xCoord) + yCoord;
                    If[(*If y is indeterminate or infinity*)And @@ (FreeQ[
                        {grad, yCoord, tangent}, #]& /@ {Indeterminate, ComplexInfinity, DirectedInfinity
                        }),
                        result = spd`Private`spdEquPlotG[tangent, "Tangent", Column
                             @ {Column @ {"Equation:", Defer @ equ}, Column @ {"Gradient:", grad}
                            }, dom]
                        ,
                        result = {}
                    ];
                    Unprotect[spdFindTangent];
                    spdFindTangent[Unevaluated @ equ, xCoord, dom] = 
                        result;
                    Protect[spdFindTangent];
                    Return[result, spdFindTangent]
                }
            ]
    };


(*Find normal*)

spdFindNormal[equ_, xCoord_, dom_:Global`x] :=
    {
        Quiet @
            Module[{grad, yCoord, normal, result},
                {
                    (*Find gradient*)grad = -1 / D[equ, dom] /. dom ->
                         xCoord;
                    (*Find yCoord*)
                    yCoord = equ /. dom -> xCoord;
                    (*Find normal*)
                    normal = grad (dom - xCoord) + yCoord;
                    If[(*If y is indeterminate or infinity*)And @@ (FreeQ[
                        {grad, yCoord, normal}, #]& /@ {Indeterminate, ComplexInfinity, DirectedInfinity
                        }),
                        result = spd`Private`spdEquPlotG[normal, "Normal", Column
                             @ {Column @ {"Equation:", Defer @ equ}, Column @ {"Gradient:", grad}
                            }, dom]
                        ,
                        result = {}
                    ];
                    Unprotect[spdFindNormal];
                    spdFindNormal[Unevaluated @ equ, xCoord, dom] = result
                        ;
                    Protect[spdFindNormal];
                    Return[result, spdFindNormal]
                }
            ]
    };


(*Find End Points*)

spdEndPoints[equ_, dom_:Global`x, domLow_ : -10, domHigh_:10, rangeLow_ : -10,
     rangeHigh_:10] :=
    {
        Quiet @
            Module[{rules = Dispatch[{Inequality -> List, Or -> List,
                 Less -> Nothing, LessEqual -> List, Greater -> Nothing, GreaterEqual
                 -> List, True -> List, Null -> List, Floor @ domLow - 1 -> Sequence,
                 Ceiling @ domHigh + 1 -> Sequence}], epX = FunctionDomain[Unevaluated
                 @ equ, dom, Reals], epY, edges, result},
                {
                    (*Find if edges exist using the solve method*)edges
                         =
                        DeleteDuplicates @
                            {
                                (*Left Wall*)Map[{domLow, #, D[Unevaluated
                                     @ equ, dom] /. dom -> domLow}&, y /. Solve[y == Unevaluated @ equ &&
                                     rangeLow <= y <= rangeHigh /. dom -> domLow, y, Reals]]
                                ,
                                (*Right Wall*)
                                Map[{domHigh, #, D[Unevaluated @ equ,
                                     dom] /. dom -> domHigh}&, y /. Solve[y == Unevaluated @ equ && rangeLow
                                     <= y <= rangeHigh /. dom -> domHigh, y, Reals]]
                                ,
                                (*Top Wall*)
                                Map[{#, rangeLow, D[Unevaluated @ equ,
                                     dom] /. dom -> #}&, dom /. Solve[rangeLow == Unevaluated @ equ && domLow
                                     <= dom <= domHigh, dom, Reals]]
                                ,
                                (*Bottom Wall*)
                                Map[{#, rangeHigh, D[Unevaluated @ equ,
                                     dom] /. dom -> #}&, dom /. Solve[rangeHigh == Unevaluated @ equ && domLow
                                     <= dom <= domHigh, dom, Reals]]
                            };
                    (*Get every expression possible*)
                    epX = Level[epX, {0, \[Infinity]}];
                    (*Parse out everything not less or greater*)
                    epX = Table[Select[epX, Head @ # === i&], {i, {LessEqual,
                         GreaterEqual, Unequal}}];
                    (*Get only numbers from expression*)
                    epX = DeleteDuplicates @ Select[Level[epX, {0, \[Infinity]}
                        ], NumericQ];
                    (*Find the y coords of each xCoord*)
                    epY = Table[Limit[equ, dom -> epX[[$i]]], {$i, Length[
                        epX]}];
                    (*All accept lists that are 3 long*)
                    edges = Select[Flatten[edges, 1], Length @ # == 3&
                        ];
                    (*Remove points if any of the 3 points are not numeric
                        *)
                    edges = Select[edges, AllTrue[#, NumericQ]&];
                    (*Combine both methods of calculating points*)
                    edges = Flatten[{edges, Transpose @ {epX, epY, Table[
                        "Indeterminate", Length[epX]]}}, 1];
                    result = {{Defer @ equ}, edges};
                    Unprotect[spdEndPoints];
                    spdEndPoints[Unevaluated @ equ, dom, domLow, domHigh,
                         rangeLow, rangeHigh] = result;
                    Protect[spdEndPoints];
                    Return[result, spdEndPoints]
                }
            ]
    };


(*Must be input with Unevaluated@expr*)

spdHoles[expr_, dom_:Global`x] :=
    {
        Quiet @
            Module[{holeX = FunctionDomain[Unevaluated @ expr, dom, Reals
                ], holeY, rules = Dispatch[{\[Infinity] -> Indeterminate, -\[Infinity] -> Indeterminate}],
                 result, combine},
                {
                    (*Get every expression possible*)holeX = Level[holeX,
                         {0, \[Infinity]}];
                    (*Parse out everything not less or greater*)
                    holeX = Table[Select[holeX, Head @ # === i&], {i,
                         {Less, Greater}}];
                    (*Get only numbers from expression*)
                    holeX = DeleteDuplicates @ Select[Level[holeX, {0,
                         \[Infinity]}], NumericQ];
                    (*If not a single x point is found, return nothing
                        *)
                    If[Flatten @ holeX == {},
                        Return[{}, spdHoles]
                    ];
                    (*Find the y coordinates that don't exist*)
                    holeY = Map[Limit[expr, dom -> #]&, holeX];
                    (*Combine holeX and holeY*)
                    combine = Transpose @ {holeX, holeY, Table["Its a Hole",
                         Length[holeX]]};
                    (*Filter out all indeterminate and infinite points
                        *)
                    combine = Select[combine /. rules, #[[2]] =!= Indeterminate&
                        ];
                    result = {Defer @ expr, combine};
                    Unprotect[spdHoles];
                    spdHoles[Unevaluated @ expr, dom] = result;
                    Protect[spdHoles];
                    Return[result, spdHoles]
                }
            ]
    };


              (*-------------------------Objects-------------------------
Compute points with tooltip    How did this just go missing in version21?????*)

spdPointsG[points_, label_, colour_:spdColourPoint1] :=
    Graphics[
        Table[
            Table[
                (*Create a string of data to display*)DynamicModule[
                    {
                        tooltipString =
                            spdStyle @
                                Column @
                                    {
                                        label
                                        ,
           (*$i hate all of these brackets with all my heart,
and $i will never be able to debug this in the future*)
                                        TableForm @ {"Equation:", points
                                            [[point]][[1]]}
                                        ,
                                        TableForm @ {{"X:", N[points[[
                                            point]][[2]][[point2]][[1]], spdRoundPrecision], Rationalize[points[[
                                            point]][[2]][[point2]][[1]]]}, {"Y:", N[points[[point]][[2]][[point2]]
                                            [[2]], spdRoundPrecision], Rationalize @ Rationalize[points[[point]][[
                                            2]][[point2]][[2]]]}, {"Grad:", N[points[[point]][[2]][[point2]][[3]],
                                             spdRoundPrecision], Simplify[points[[point]][[2]][[point2]][[3]]]}}
                                    }
                    }
                    ,
                    {
                        (*Event handler can handle clicks*)EventHandler[
                            
                            (*Convert into tooltip form*)Tooltip[
                                {{PointSize[Large], Darker @ colour, 
                                    Point[N @ {points[[point]][[2]][[point2]][[1]], points[[point]][[2]][[
                                    point2]][[2]]}]}, {PointSize[Medium], Lighter @ Lighter @ colour, Point[
                                    N @ {points[[point]][[2]][[point2]][[1]], points[[point]][[2]][[point2
                                    ]][[2]]}]}}
                                ,
                                (*Give coordinate when you hover over
                                     location*)
                                tooltipString // spdForm
                            ]
                            ,
                            (*If a click is detected on point, print 
                                whatever is in tooltip*)
                            {"MouseClicked" :> Print @ Framed[tooltipString,
                                 Background -> LightYellow]}
                        ]
                    }
                ]
                ,
                (*Iterate over every point*)
                {point2, Length[points[[point]][[2]]]}
            ]
            ,
            (*Iterate over every equation*)
            {point, Length[points]}
        ]
    ];


spdPointsGPOI[points_, label_, colour_:spdColourPoint1] :=
    Graphics[
        Table[
            Table[
                (*Create a string of data to display*)DynamicModule[
                    {
                        tooltipString =
                            spdStyle[
                                Column @
                                    {
                                        label
                                        ,
           (*$i hate all of these brackets with all my heart,
and $i will never be able to debug this in the future*)
                                        TableForm @ {{"Equation:", Style[
                                            points[[point]][[1]][[1]], Background -> Lighter @ spdColourPoint1], 
                                            Style[points[[point]][[1]][[2]], Background -> Lighter @ spdColourPoint2
                                            ]}, {"X:", N[points[[point]][[2]][[point2]][[1]], spdRoundPrecision],
                                             Rationalize @ points[[point]][[2]][[point2]][[1]]}, {"Y:", N[points[[
                                            point]][[2]][[point2]][[2]], spdRoundPrecision], Rationalize @ points
                                            [[point]][[2]][[point2]][[2]]}, {"Grad:", Style[Column @ {N[points[[point
                                            ]][[2]][[point2]][[3]][[1]], spdRoundPrecision], Simplify @ points[[point
                                            ]][[2]][[point2]][[3]][[1]]}, Background -> Lighter @ spdColourPoint1
                                            ], Style[Column @ {N[points[[point]][[2]][[point2]][[3]][[2]], spdRoundPrecision
                                            ], Simplify @ points[[point]][[2]][[point2]][[3]][[2]]}, Background ->
                                             Lighter @ spdColourPoint2]}}
                                    }
                            ]
                    }
                    ,
                    {
                        (*Event handler can handle clicks*)EventHandler[
                            
                            (*Convert into tooltip form*)Tooltip[
                                {{PointSize[Large], Darker @ colour, 
                                    Point[N @ {points[[point]][[2]][[point2]][[1]], points[[point]][[2]][[
                                    point2]][[2]]}]}, {PointSize[Medium], Lighter @ Lighter @ colour, Point[
                                    N @ {points[[point]][[2]][[point2]][[1]], points[[point]][[2]][[point2
                                    ]][[2]]}]}}
                                ,
                                (*Give coordinate when you hover over
                                     location*)
                                tooltipString // spdForm
                            ]
                            ,
                            (*If a click is detected on point, print 
                                whatever is in tooltip*)
                            {"MouseClicked" :> Print @ Framed[tooltipString,
                                 Background -> LightYellow]}
                        ]
                    }
                ]
                ,
                (*Iterate over every point*)
                {point2, Length[points[[point]][[2]]]}
            ]
            ,
            (*Iterate over every equation*)
            {point, Length[points]}
        ]
    ];


(*Compute lines with tooltip*)

spdLinesG[points_, colour_:spdColourLine] :=
    Graphics[
        Table[
            Table[
                (*Create a string of data to display*)DynamicModule[
                    {tooltipString = spdStyle[TableForm @ points[[point
                        ]][[1]]]}
                    ,
                    {
                        (*Event handler can handle clicks*)EventHandler[
                            
                            (*Convert into tooltip form*)(*This is disgusting
                                *)Tooltip[
                                {Dashed, colour, InfiniteLine[N @ points
                                    [[point]][[2]]]}
                                ,
                                (*Give coordinate when you hover over
                                     location*)
                                tooltipString // spdForm
                            ]
                            ,
                            (*If a click is detected on point, print 
                                whatever is in tooltip*)
                            {"MouseClicked" :> Print @ Framed[tooltipString,
                                 Background -> LightYellow]}
                        ]
                    }
                ]
                ,
                (*Iterate over every line*)
                {point2, Length[points[[point]][[2]]]}
            ]
            ,
            (*Iterate over every equation*)
            {point, Length[points]}
        ]
    ];

       (*Idk why this has to be evaluated
$i made it work but its messy code*)

spdEquPlotG[equ_, message_:"Equation", message2_:"", dom_:Global`x] :=
    {
        Module[{head, context},
            {
                If[Flatten @ {equ} == {},
                    (*If equation is empty, make it return nothing*)
                    spdEquPlotG[Unevaluated @ equ, message, message2,
                         dom] = {}
                    ,
                    spdEquPlotG[Unevaluated @ equ, message, message2,
                         dom] =
                        (*Simple error checking*)
                        With[{equString = spdStyle[Column @ {message,
                             Extract[Hold @ equ, 1, Defer], message2, TableForm @ {Column @ {"Domain",
                             spdErrorChecker @ Unevaluated @ Simplify @ FunctionDomain[equ, dom, 
                            Reals]}, Column @ {"Range", spdErrorChecker @ Unevaluated @ Simplify 
                            @ FunctionRange[equ, dom, Global`y, Reals]}} /. Dispatch[True -> Reals]}]},
                            EventHandler[Tooltip[Evaluate @ equ, equString
                                 // spdForm // ReleaseHold], "MouseClicked" :> Print @ Framed[ReleaseHold
                                 @ equString, Background -> LightYellow]]
                        ]
                ]
            }
        ]
    };

(*I hate this code but its necessary*)

spdEquPlotGInverse[equ_, message_:"Equation", message2_:"", dom_:Global`x] :=
    {
        Module[{head, context},
            {
                If[Flatten @ {equ} == {},
                    (*If equation is empty, make it return nothing*)
                    spdEquPlotGInverse[Unevaluated @ equ, message, message2,
                         dom] = {}
                    ,
                    spdEquPlotGInverse[Unevaluated @ equ, message, message2,
                         dom] =
                        (*Simple error checking*)  (*I hate this extract
                             code but it works so well*)
                        With[{equString = spdStyle[Column @ {message,
                             message2, Extract[Hold @ equ, 1, Defer], TableForm @ {Column @ {"Domain",
                             spdErrorChecker @ Unevaluated @ FunctionRange[equ, dom, Global`y, Reals]}, 
                            Column @ {"Range", spdErrorChecker @ Unevaluated @ FunctionDomain[equ,
                             dom, Reals]}} /. Dispatch[True -> Reals]}]},
                            EventHandler[Tooltip[{Evaluate @ equ, dom
                                }, equString // spdForm], "MouseClicked" :> Print @ Framed[ReleaseHold
                                 @ equString, Background -> LightYellow]]
                        ]
                ]
            }
        ]
    };


(*Plot equations after hovering over them*)

spdEquPreview[equ_, size_:Small] :=
    TableForm[{Tooltip[spdForm @ equ, spdPlotFast[equ, ImageSize -> size
        ]], spdCopyThis[equ]}, TableDirections -> Row];

(*Predefined style function*)

spdStyle = Style[#, FontFamily -> spdFont, FontSize -> Larger]&;

(*-------------------------Plot-------------------------*)

Options[spdPlot] = Options[Plot];


           (*$i like this plot
$i don't care too much about commenting this but It Just Works*)

spdPlotFast[equ_, dom : {_, _?NumericQ, _?NumericQ} : {Global`x, -10, 10}, plotOptions
     : OptionsPattern[]] :=
   spdPlotFaster[Evaluate[spd`Private`spdEquPlotG[equ]], dom, plotOptions
        ];


spdPlotDerivative[equ_, dom : {_, _?NumericQ, _?NumericQ} : {Global`x, -10, 
    10}, plotOptions : OptionsPattern[]] :=
    {
        Module[{
            (*Turn equations into a standard table form that can be used
                *)equList = Flatten @ {equ}, equListD, result
        },
            {
                equListD = Simplify @ Map[D[#, dom[[1]]]&, equList];
                result = Plot[Evaluate @ Table[spd`Private`spdEquPlotG[Evaluate @
                     equListD[[$i]], "Derivative", Column @ {"Original Equation", equList
                    [[$i]] /. Unevaluated -> Defer}, dom[[1]]], {$i, Length[equList]}], dom,
                     Prolog -> {}, Evaluate @ FilterRules[{plotOptions}, Options @ spdPlot
                    ], PlotStyle -> Dashed, Evaluate @ spd`Private`spdAltPlotLabel["Derivative", Length[
                    equList]]];
                Return[result, spdPlotDerivative]
            }
        ]
    };


spdPlotIntegrate[equ_, dom : {_, _?NumericQ, _?NumericQ} : {Global`x, -10, 10
    }, plotOptions : OptionsPattern[]] :=
    {
        Module[{equList = Flatten @ {equ}, equListI, result},
            {
                equListI = Map[Integrate[#, dom[[1]]]&, equList];
                result = Plot[Evaluate @ Table[spd`Private`spdEquPlotG[Evaluate @
                     equListI[[$i]], "Integral", Column @ {"Original Equation", equList[[
                    $i]] /. Unevaluated -> Defer}, dom[[1]]], {$i, Length[equList]}], dom,
                     Prolog -> {}, Evaluate @ FilterRules[{plotOptions}, Options @ spdPlot
                    ], PlotStyle -> Dashed, Evaluate @ spd`Private`spdAltPlotLabel["Integral", Length[
                    equList]]];
                Return[result, spdPlotIntegrate]
            }
        ]
    };


(*Inverse plot. Thanks Dulvino*)

spdPlotInverse[equ_, dom : {_, _?NumericQ, _?NumericQ} : {Global`x, -10, 10},
     plotOptions : OptionsPattern[]] :=
    {
        Module[{
            (*Turn equations into a standard table form that can be used
                *)equList = Flatten @ {equ}, result
        },
            {
                result = ParametricPlot[Evaluate @ Table[spd`Private`spdEquPlotGInverse[
                    equList[[$i]], Column[{"Inverse", Normal @ spdInverse[equList[[$i]], 
                    dom[[1]]]}], "Original Equation", dom[[1]]], {$i, Length[equList]}], 
                    dom, Prolog -> {}, Evaluate @ FilterRules[{plotOptions}, Options @ spdPlot
                    ], PlotStyle -> Dashed, Evaluate @ spd`Private`spdAltPlotLabel["Inverse", Length[
                    equList]]];
                Return[result, spdPlotInverse]
            }
        ]
    };


(*Find the automatic plotrange of any equation*)

spdPlotInitRange[equ_, dom_ : {Global`x, -10, 10}, plotOptions : OptionsPattern[
    ]] :=
    {
        Module[{initRange = PlotRange /. plotOptions},
            {
                If[AllTrue[Flatten @ {initRange}, NumericQ],
                    {
                        (*If PlotRange is a single number, make it not
                             a single number*)If[Length[Flatten @ {initRange}] == 1,
                            initRange = Flatten @ {initRange, -initRange
                                }
                        ]
                    }
                    ,
                    {initRange = (PlotRange /. Options[Plot[Flatten @
                         {equ}, dom, PlotRange -> spdPlotRange]])[[2]]}
                ];
                Return[{Floor @ Min[initRange], Ceiling @ Max[initRange
                    ]}, spdPlotInitRange]
            }
        ]
    };


                    (*Plot all of this
This is a new way of declaring domain and range that $i'm not use to. From Joseph's GigaChadPlot of course :)*)

spdPlot[equ_:Global`x,dom:{_,_?NumericQ,_?NumericQ}:{Global`x,-10,10},plotOptions:OptionsPattern[]] :=
    {
    (*Plz stay quiet, thanks
Immuatable variables
Sometimes you have to delete DynamicModule for some unknown reason
Do not make this Quiet*)DynamicModule[
            {
            inList = Table[Hold@Null, 12],
                equList = Hold @ equ
                ,
                (*Dragging area*)
                initPos = MousePosition[]
                ,
                finalPos
                ,
                (*Range to plot over*)
                idomLow = dom[[2]]
                ,
                idomHigh = dom[[3]]
                ,
                domLow = dom[[2]]
                ,
                domHigh = dom[[3]]
                ,
                rangeLow
                ,
                rangeHigh
                ,
                irangeLow
                ,
                irangeHigh
                ,
                (*The left input bar variables*)
                findXInput = 0
                ,
                findXBool = True
                ,
                findYInput = 0
                ,
                findYBool = True
                ,
                findGradInput = 0
                ,
                findGradBool = True
                ,
                findTangentInput = 0
                ,
                findTangentBool = False
                ,
                findNormalInput = 0
                ,
                findNormalBool = False
                ,
                asyBool = False
                ,
                asyV = {{}}
                ,
                asyO = {{}}
                ,
                asyG = {{}}
                ,
                poiBool = True
                ,
                epBool = True
                ,
                holeBool = True
                ,
                inverseBool = False
                ,
                derivativeBool = False
                ,
                integrateBool = False
                ,
                size = {500, 500}
                ,
                graph = spdPlotFaster[Global`x, dom]
                ,
                notScroll = True
                ,
                graphIt
            }
            ,
            Quiet@{
            graphIt=spdPlotFast[#,{dom[[1]],domLow,domHigh},PlotRange->{rangeLow,rangeHigh},ImageSize->size,PlotLegends->None]&;
                (*Unevaluate all equations*)If[Head @ equ === List,
                    equList = Evaluate @ Table[Extract[equList, {1, i
                        }, Hold], {i, Length[equ]}]
                    ,
                    equList = {Hold @ equ}
                ];
                (*Desect any user global functions in equList*)
                equList = spd`Private`spdDesectFunction @ equList;
                (*Combine the equList into inList*)
                Table[inList[[i]]=equList[[i]],{i,Length[equList]}];
                (*Parse domain*)
                If[dom[[2]] != -10 && dom[[3]] != 10,
                    {idomLow = domLow = dom[[2]], idomHigh = domHigh 
                        = dom[[3]]}
                ];
                (*Round to whole numbers to allow fractions*)
                {rangeLow, rangeHigh} = spd`Private`spdPlotInitRange[
                    equ, {dom[[1]], domLow, domHigh}, plotOptions];
                irangeLow = rangeLow;
                irangeHigh = rangeHigh;
    (*Calculate variables as needed
Calculate the 6 variables below first because they seem more important
    
    
    
    *)
                asyV =
                    Dynamic[
                        If[asyBool && notScroll,
                            spdAsyV[equList[[1]], dom[[1]], domLow, domHigh
                                ]
                        ]
                        ,
                        SynchronousUpdating -> False
                        ,
                        TrackedSymbols :> {asyBool, domLow, domHigh,equList}
                    ];
                asyO =
                    Dynamic[
                        If[asyBool && notScroll,
                            spdAsyO[equList[[1]], dom[[1]]]
                        ]
                        ,
                        SynchronousUpdating -> False
                        ,
                        TrackedSymbols :> {asyBool,equList}
                    ];
                asyG =
                    {
                        (*Vertical Asymptote Lines*)Dynamic[
                            If[asyBool && notScroll,
                                spd`Private`spdLinesG[asyV[[1]], spdColourLine
                                    ]
                            ]
                            ,
                            SynchronousUpdating -> False
                            ,
                            TrackedSymbols :> {asyV}
                        ]
                        ,
                        (*Vertical Asymptote Points*)
                        Dynamic[
                            If[asyBool && notScroll,
                                spd`Private`spdPointsG[
                                    Complement[
                                            (*Find all points over all
                                            
                                            
                                             equations
                                            *)Flatten[Map[spdFindX[equList[[1]],
     #, dom[[1]], rangeLow, rangeHigh]&, spd`Private`spdAsyParser[asyV[[1
    ]]]], 1], {{}}
                                    ]
                                    ,
                                    "Vertical Asymptote"
                                    ,
                                    spdColourLine
                                ]
                            ]
                            ,
                            SynchronousUpdating -> False
                            ,
                            TrackedSymbols :> {asyV,equList}
                        ]
                        ,
                        (*Other Asymptotes*)
                        Dynamic[
                            If[asyBool && notScroll,
                                spdPlotFaster[asyO[[1]], {dom[[1]], domLow,
                                     domHigh}, PlotRange -> {rangeLow, rangeHigh}, PlotStyle -> Dashed]
                            ]
                            ,
                            SynchronousUpdating -> False
                            ,
                            TrackedSymbols :> {asyO}
                        ]
                        ,
                        (*Other Asymptote Points of Intersection*)
                        Dynamic[
                            If[asyBool && notScroll,
                                spd`Private`spdPointsGPOI[
                                           (*Find all asymptote and intersection
                                        
                                        
                                        
                                         points of intersection*)Flatten[
    
                                        (*Iterate over all equations*)
                                            Map[
                                                (*Find points of insection
                                                
                                                
                                                
                                                *)spdPOI[
                                                Flatten @
                                                    {
    (*Parse
                                                             data 
                                                            out of asymptotes
    
    
    *)Map[
                                                            {#[[1]][[
                                                                1]][[1]][[1]][[1]]}&
                                                            ,
     (*Remove
                                                                 
                                                                all empty
    
    
     points*)
                                                            Complement[
                                                                spdAsyO[equList[[1]], dom[[1]]], {{}}]
                                                        ]
                                                        ,
                                                        #
                                                    }
                                                ,
                                                dom[[1]]
                                                ,
                                                domLow
                                                ,
                                                domHigh
                                                ,
                                                rangeLow
                                                ,
                                                rangeHigh
                                            ]&
                                            ,
                                            equList[[1]]
                                        ]
                                        ,
                                        1
                                    ]
                                    ,
                                    "Other Asymptote"
                                    ,
                                    spdColourLine
                                ]
                            ]
                            ,
                            SynchronousUpdating -> False
                            ,
                            TrackedSymbols :> {asyO,equList}
                        ]
                    };
                    
                (*Core plotting function*)
                spdStyle @
                    TableForm[
                        {
     (*Pseudo Manipulate area
Map to make all lists rows*)Row @
                                {
                                    TableForm[
                                        Map[
                                            If[Head[#] === List,
                                                Row @ #
                                                ,
                                                #
                                            ]&
                                            ,
                                            {
                                                "Domain"
                                                ,
                                                {
                                                    spd`Private`spdInputField[
                                                        
                                                        Dynamic[
                                                            domLow
                                                            ,
                                                            (
                                                                domLow
                                                                     = #;
                                                                graph
                                                                     = graphIt[equList[[1]]]
                                                            )&
                                                            ,
                                                            SynchronousUpdating
                                                                 -> False
                                                        ]
                                                        ,
                                                        Tiny
                                                    ]
                                                    ,
                                                    spd`Private`spdInputField[
                                                        
                                                        Dynamic[
                                                            domHigh
                                                            ,
                                                            (
                                                                domHigh
                                                                     = #;
                                                                graph
                                                                     = graphIt[equList[[1]]]
                                                            )&
                                                            ,
                                                            SynchronousUpdating
                                                                 -> False
                                                        ]
                                                        ,
                                                        Tiny
                                                    ]
                                                }
                                                ,
                                                "Range"
                                                ,
                                                {
                                                    spd`Private`spdInputField[
                                                        
                                                        Dynamic[
                                                            rangeLow
                                                            ,
                                                            (
                                                                rangeLow
                                                                     = #;
                                                                graph
                                                                     = graphIt[equList[[1]]]
                                                            )&
                                                            ,
                                                            SynchronousUpdating
                                                                 -> False
                                                        ]
                                                        ,
                                                        Tiny
                                                    ]
                                                    ,
                                                    spd`Private`spdInputField[
                                                        
                                                        Dynamic[
                                                            rangeHigh
                                                                
                                                            ,
                                                            (
                                                                rangeHigh
                                                                     = #;
                                                                graph
                                                                     = graphIt[equList[[1]]]
                                                            )&
                                                            ,
                                                            SynchronousUpdating
                                                                 -> False
                                                        ]
                                                        ,
                                                        Tiny
                                                    ]
                                                }
                                                ,
                                                {Checkbox[Dynamic[findXBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Find X"}
                                                    
                                                ,
                                                spd`Private`spdInputField[
                                                    Dynamic[findXInput, SynchronousUpdating -> False], Small]
                                                ,
                                                {Checkbox[Dynamic[findYBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Find Y"}
                                                    
                                                ,
                                                spd`Private`spdInputField[
                                                    Dynamic[findYInput, SynchronousUpdating -> False], Small]
                                                ,
                                                {Checkbox[Dynamic[findGradBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Find Gradient"
                                                    }
                                                ,
                                                spd`Private`spdInputField[
                                                    Dynamic[findGradInput, SynchronousUpdating -> False], Small]
                                                ,
                                                {Checkbox[Dynamic[findTangentBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Find Tangent at X"
                                                    }
                                                ,
                                                spd`Private`spdInputField[
                                                    Dynamic[findTangentInput, SynchronousUpdating -> False], Small]
                                                ,
                                                {Checkbox[Dynamic[findNormalBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Find Normal at X"
                                                    }
                                                ,
                                                spd`Private`spdInputField[
                                                    Dynamic[findNormalInput, SynchronousUpdating -> False], Small]
                                                ,
                                                      (*Checkbox for 
                                                    
                                                    
                                                    points
                                                     of intersection*)
    
                                                {Checkbox[Dynamic[poiBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Points of Intersect"
                                                    }
                                                ,
                                                {Checkbox[Dynamic[epBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "End Points"
                                                    }
                                                ,
                                                {Checkbox[Dynamic[holeBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Holes"}
                                                ,
                                                     (*Checkbox for toggling
                                                    
                                                    
                                                    
                                                     asymptotes*)
                                                {Checkbox[Dynamic[asyBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Asymptotes"
                                                    }
                                                ,
                                                {Checkbox[Dynamic[inverseBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Inverse"
                                                    }
                                                ,
                                                {Checkbox[Dynamic[derivativeBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Derivative"
                                                    }
                                                ,
                                                {Checkbox[Dynamic[integrateBool,
                                                     SynchronousUpdating -> False], Appearance -> spdPlotSize], "Integrate"
                                                    }
                                            }
                                        ]
                                        ,
                                        TableSpacing -> {0, 0}
                                    ]
                                    ,
                                    Pane[
                                        EventHandler[
                                            Dynamic[
                                                Show[
                                                    graph
                                                    ,
        (*Inverse
                                                         and derivative
    
    
                                                         plots. These
    
    
     can be behind everything*)
                                                    If[notScroll,
                                                        {
                                                            If[inverseBool,
                                                                
                                                                Dynamic[
                                                                    
                                                                        spdPlotInverse[
                                                                            equList[[1]], {dom[[1]], rangeLow, rangeHigh}, PlotRange -> {domLow, domHigh
                                                                            }, plotOptions]
                                                                    
                                                                        
                                                                    ,
                                                                        
                                                                    SynchronousUpdating
                                                                         -> False
                                                                    ,
                                                                        
                                                                    TrackedSymbols
                                                                         :> {inverseBool, domLow, domHigh, rangeLow, rangeHigh,equList}
                                                                ][[1]]
                                                                    
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[derivativeBool,
                                                                
                                                                Dynamic[
                                                                    spdPlotDerivative[equList[[1]], {dom[[1]], domLow, domHigh}, PlotRange -> 
                                                                    {rangeLow, rangeHigh}, plotOptions], SynchronousUpdating -> False, TrackedSymbols
                                                                     :> {derivativeBool, domLow, domHigh, rangeLow, rangeHigh,equList}][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[integrateBool,
                                                                
                                                                Dynamic[
                                                                    spdPlotIntegrate[equList[[1]], {dom[[1]], domLow, domHigh}, PlotRange -> {
                                                                    rangeLow, rangeHigh}, plotOptions], SynchronousUpdating -> False, TrackedSymbols
                                                                     :> {integrateBool, domLow, domHigh, rangeLow, rangeHigh,equList}][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
         (*Plot asymptotes after that, since they are long*)
                                                            If[asyBool,
                                                                
                                                                asyG[[
                                                                    3]][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[asyBool,
                                                                
                                                                {asyG[[1]][[1]], asyG[[2]][[1]], asyG[[4]][[1]]}
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                (*Find these points on the graph before poi and cp, since Find is less important*)
                                                            If[epBool,
                                                                
                                                                Dynamic[
                                                                    spd`Private`spdPointsG[spdEndPoints[equList[[1]], dom[[1]], Rationalize @ 
                                                                    domLow, Rationalize @ domHigh, Rationalize @ rangeLow, Rationalize @ 
                                                                    rangeHigh], "End Point", spdColourPoint3], SynchronousUpdating -> False,
                                                                     TrackedSymbols :> {epBool, domLow, domHigh, rangeLow, rangeHigh,equList}][[1
                                                                    ]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
        (*Points and plot have to be independent*)
                                                            If[findTangentBool,
                                                                Dynamic[
                                                                spdPlotFaster[
                                                                    spdFindTangent[equList[[1]], findTangentInput, dom[[1]]], {dom[[1]], domLow,
                                                                     domHigh}, PlotStyle -> {Dashed, spdColourLine},PlotLegends->None],SynchronousUpdating->False,TrackedSymbols:>{equList}][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findTangentBool,
                                                                Dynamic[
                                                                spd`Private`spdPointsG[
                                                                    Complement[spdFindX[equList[[1]], findTangentInput, dom[[1]], rangeLow, rangeHigh
                                                                    ], {{}}], "Find Tangent", spdColourLine],SynchronousUpdating->False,TrackedSymbols:>{equList}][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findNormalBool,
                                                                Dynamic[
                                                                spdPlotFaster[
                                                                    spdFindNormal[equList[[1]], findNormalInput, dom[[1]]], {dom[[1]], domLow,
                                                                     domHigh}, PlotStyle -> {Dashed, spdColourLine}, PlotLegends->None],SynchronousUpdating->False,TrackedSymbols:>{equList}][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findNormalBool,
                                                                Dynamic[
                                                                spd`Private`spdPointsG[
                                                                    Map[spd`Private`spdInverseGradient @ #&, Complement[spdFindX[equList[[1]],
                                                                     findNormalInput, dom[[1]], rangeLow, rangeHigh], {{}}]], "Find Normal",
                                                                     spdColourLine],SynchronousUpdating->False,TrackedSymbols:>{equList}][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findXBool,
                                                                
                                                                Dynamic[
                                                                    spd`Private`spdPointsG[Complement[spdFindX[equList[[1]], findXInput, dom[[
                                                                    1]], rangeLow, rangeHigh], {{}}], "Find X", spdColourPoint2], SynchronousUpdating
                                                                     -> False, TrackedSymbols :> {findXInput, findXBool, rangeLow, rangeHigh,equList
                                                                    }][[1]],
                                                                    {}
                                                            ]
                                                            ,
                                                            If[findYBool,
                                                                
                                                                Dynamic[
                                                                    
                                                                        spd`Private`spdPointsG[
                                                                            Complement[spdFindY[equList[[1]], findYInput, dom[[1]], domLow, domHigh], 
                                                                            {{}}], "Find Y", spdColourPoint2]
                                                                    
                                                                        
                                                                    ,
                                                                        
                                                                    SynchronousUpdating
                                                                         -> False
                                                                    ,
                                                                        
                                                                    TrackedSymbols
                                                                         :> {findYInput, findYBool, domLow, domHigh,equList}
                                                                ][[1]]
                                                                    
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findGradBool,
                                                                
                                                                Dynamic[
                                                                    
                                                                        spd`Private`spdPointsG[
                                                                            Complement[spdFindGrad[equList[[1]], findGradInput, dom[[1]], domLow, domHigh,
                                                                             rangeLow, rangeHigh], {{}}], "Find Gradient", spdColourPoint1]
                                                                    
                                                                        
                                                                    ,
                                                                        
                                                                    SynchronousUpdating
                                                                         -> False
                                                                    ,
                                                                        
                                                                    TrackedSymbols
                                                                         :> {findGradInput, findGradBool, domLow, domHigh, rangeLow, rangeHigh,equList
                                                                        }
                                                                ][[1]]
                                                                    
                                                                ,
                                                                {}
                                                            ]
                                                            ,
        (*Plot points next, since they are small*)
                                                            If[poiBool,
                                                                
                                                                Dynamic[
                                                                    spd`Private`spdPointsGPOI[spdPOI[Evaluate/@equList[[1]], dom[[1]], domLow, domHigh,
                                                                     rangeLow, rangeHigh], "Point of Intersection", spdColourPoint1], SynchronousUpdating
                                                                     -> False, TrackedSymbols :> {poiBool, domLow, domHigh, rangeLow, rangeHigh,equList
                                                                    }][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[holeBool,
                                                                
                                                                Dynamic[
                                                                    spd`Private`spdPointsG[Complement[spdHoles[equList[[1]], dom[[1]]], {{}}],
                                                                     "Hole", spdColourPoint4], SynchronousUpdating -> False, TrackedSymbols
                                                                     :> {holeBool,equList}][[1]]
                                                                ,
                                                                {}
                                                            ]
                                                        }
                                                        ,
                                                        (*_______________________________Not Scroll False_________________________________*)
                                                        {
                                                            If[inverseBool,
                                                            spdPlotInverse[
                                                                   equList[[1]], {dom[[1]], irangeLow, irangeHigh}, PlotRange -> {idomLow, idomHigh
                                                            }, plotOptions],
                                                            {}]
                                                            ,
                                                            
                                                            If[derivativeBool,
                                                                    spdPlotDerivative[equList[[1]], {dom[[1]], idomLow, idomHigh}, PlotRange -> 
                                                                    {irangeLow, irangeHigh}, plotOptions]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[integrateBool,
                                                                
                                                                    spdPlotIntegrate[equList[[1]], {dom[[1]], idomLow, idomHigh}, PlotRange -> {
                                                                    irangeLow, irangeHigh}, plotOptions]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                (*Find these points on the graph before poi and cp, since Find is less important*)
                                                            If[epBool,
                                                                    spd`Private`spdPointsG[spdEndPoints[equList[[1]], dom[[1]], Rationalize @ 
                                                                    idomLow, Rationalize @ idomHigh, Rationalize @ irangeLow, Rationalize @ 
                                                                    irangeHigh], "End Point", spdColourPoint3]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
        (*Points and plot have to be independent*)
                                                            If[findTangentBool,
                                                                spdPlotFaster[
                                                                    spdFindTangent[equList[[1]], findTangentInput, dom[[1]]], {dom[[1]], idomLow,
                                                                     idomHigh}, PlotStyle -> {Dashed, spdColourLine}, PlotLegends->None]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findTangentBool,
                                                                spd`Private`spdPointsG[
                                                                    Complement[spdFindX[equList[[1]], findTangentInput, dom[[1]], irangeLow, irangeHigh
                                                                    ], {{}}], "Find Tangent", spdColourLine]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findNormalBool,
                                                                
                                                                spdPlotFaster[
                                                                    spdFindNormal[equList[[1]], findNormalInput, dom[[1]]], {dom[[1]], idomLow,
                                                                     idomHigh}, PlotStyle -> {Dashed, spdColourLine}, PlotLegends->None]
                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[findNormalBool,
                                                                spd`Private`spdPointsG[
                                                                    Map[spd`Private`spdInverseGradient @ #&, Complement[spdFindX[equList[[1]],
                                                                     findNormalInput, dom[[1]], irangeLow, irangeHigh], {{}}]], "Find Normal",
                                                                     spdColourLine],
                                                                {}
                                                            ]
                                                            ,
                                                            If[findXBool,
                                                                
                                                                    spd`Private`spdPointsG[Complement[spdFindX[equList[[1]], findXInput, dom[[
                                                                    1]], irangeLow, irangeHigh], {{}}], "Find X", spdColourPoint2],
                                                                    {}
                                                            ]
                                                            ,
                                                            If[findYBool,
                                                                
                                                                        spd`Private`spdPointsG[
                                                                            Complement[spdFindY[equList[[1]], findYInput, dom[[1]], idomLow, idomHigh], 
                                                                            {{}}], "Find Y", spdColourPoint2],
                                                                {}
                                                            ]
                                                            ,
                                                            If[findGradBool,
                                                                
                                                                        spd`Private`spdPointsG[
                                                                            Complement[spdFindGrad[equList[[1]], findGradInput, dom[[1]], idomLow, idomHigh,
                                                                             irangeLow, irangeHigh], {{}}], "Find Gradient", spdColourPoint1],
                                                                {}
                                                            ]
                                                            ,
        (*Plot points next, since they are small*)
                                                            If[poiBool,
                                                                
                                                                    spd`Private`spdPointsGPOI[spdPOI[Evaluate/@equList[[1]], dom[[1]], idomLow, idomHigh,
                                                                     irangeLow, irangeHigh], "Point of Intersection", spdColourPoint1],                                                                ,
                                                                {}
                                                            ]
                                                            ,
                                                            If[holeBool,
                                                                
                                                                    spd`Private`spdPointsG[Complement[spdHoles[equList[[1]], dom[[1]]], {{}}],
                                                                     "Hole", spdColourPoint4],
                                                                {}
                                                            ]
                                                        }
                                                    ]
                                                ]
                                                ,
    (*Weird quirk to make ] better
Dynamic Options
Plz don't process anything here*)
                                                None
                                                ,
        (*These are
                                                     symbols to
                                                     track. Don't keep
    
    
     track of anything else*)
                                                TrackedSymbols :> {graph,
                                                     findXInput, findXBool, findYInput, findYBool, findGradInput, findGradBool,
                                                     findTangentInput, findTangentInput, findTangentBool, findNormalInput,
                                                     findNormalBool, asyBool, asyG, poiBool, epBool, holeBool, inverseBool,
                                                     derivativeBool, integrateBool}
                                                ,
                                                       (*THE GOLDEN PIECE
                                                    
                                                    
                                                    , this
                                                     only run once*)
                                                CachedValue -> ToBoxes[
                                                    spdPlotFast[Rationalize @ equ, dom, {PlotRange -> {rangeLow, rangeHigh
                                                    },ImageSize->size,PlotLegends->None, plotOptions}]]
                                                ,
                                                SynchronousUpdating ->
                                                     False
                                            ]
                                            ,
                                            {
                                                PassEventsUp -> False
                                                    
                                                ,
                                                Method -> "Queued"
                                                ,
                                                "MouseDown" :>
                                                    (
                                                        initPos = MousePosition[
                                                            ];
                                                        idomLow = domLow
                                                            ;
                                                        idomHigh = domHigh
                                                            ;
                                                        irangeLow = rangeLow
                                                            ;
                                                        irangeHigh = 
                                                            rangeHigh;
                                                        notScroll = False
                                                            
                                                    )
                                                ,
                                                "MouseDragged" :>
                                                    (
													If[Head[initPos] =!= List, initPos = MousePosition[]];
                                                        finalPos = MousePosition[
                                                            ];
                                                       With[{
                                                       idomDiff=-spdDragSens(idomLow-idomHigh),
                                                       posDiff=initPos-finalPos,
                                                       irangeDiff=spdDragSens(irangeHigh-irangeLow)
                                                       },
                                                        {domLow = idomDiff * posDiff[[1]] + idomLow;
                                                        domHigh = idomDiff * posDiff[[1]] + idomHigh;
                                                        rangeLow = irangeDiff * -posDiff[[2]] + irangeLow;
                                                        rangeHigh = irangeDiff * -posDiff[[2]] + irangeHigh;
                                                             }];
                                                        graph = spdPlotFaster[Evaluate/@equList[[1]],{dom[[1]],domLow,domHigh},PlotRange->{rangeLow,rangeHigh},ImageSize->size,PlotLegends->None,PerformanceGoal->"Speed"];
                                                    )
                                                ,
                                                "MouseUp" :>
                                                    (
                                                        notScroll = True;
                                                        graph=graphIt[equList[[1]]];
                                                            
                                                    )
                                            }
                                        ]
                                        ,
                                        Dynamic[
                                            size
                                            ,
                                            (
                                                size = #;
                                                graph = graphIt[equList[[1]]
                                                    ]
                                            )&
                                            ,
                                            SynchronousUpdating -> False
                                                
                                        ]
                                        ,
                                        AppearanceElements -> "ResizeArea"
                                            
                                        ,
                                        ImageMargins -> 0
                                        ,
                                        FrameMargins -> None
                                        ,
                                        Alignment -> {Left, Center}
                                    ],
                                    Column @
                    Table[
                        With[{i = i},
							
                            InputField[
                            Dynamic[
                                    inList[[i]]
                                    ,
                                    (
                                        inList[[i]] = #;
                                        graph = graphIt[equList[[1]]]
                                    )&
                                    ,
                                    SynchronousUpdating -> False
                                ]
                                ,
                                Hold[Expression]
                                ,
                                FieldSize -> Small
                            ]
                        
                        ]
                        ,
                        {i, Length[inList]}
                    ]
                                }
                        }
                        ,
                        (*Tableform options*)
                        TableDirections -> Column
                        ,
                        TableSpacing -> {0, 0}
                    ],
                    (*Make equList dynamically update with inList*)
                equList=Dynamic[Map[Extract[#, 1, Unevaluated]&, Complement[inList,{Hold[Null]}]], SynchronousUpdating -> False, TrackedSymbols :> {inList}];
                graph = graphIt[equList[[1]]];                
            }[[1]]
        ]
    }[[1]];


Off[General::shdw];



End[];
Protect @@ Complement[Names["spd`*"],{"x"}];
(*Unprotect these letters which are protected for some reason*)
Unprotect[spd`x];
Remove[spd`x];
EndPackage[];


(*Getting rid of shadow warnings entirely was the answer*)
On[General::shdw];


(* ::Text:: *)
(*v5 Changes:*)
(*Fix crashing but with initPos*)
(*Include asymptotes inside this file*)
(*Do not display asymptotes when dragging*)
