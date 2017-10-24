(* ::Package:: *)

(* ::Input:: *)
(*(*BeginPackage["grPackage`"]*)*)


(* ::Subsection:: *)
(*Temporary*)


(* ::Input:: *)
(*ClearAll[cdgbar]*)
(*Format[cdgbar]:=OverBar["\[Del]"]*)
(**)
(*cdgbar[ind_]:=cdl[ind,cdgbar]*)
(**)
(*Unprotect[Function];*)
(*Format[cdl[#,cdgbar]&]:=OverBar@"\[Del]"*)
(*Protect[Function];*)


(* ::Section:: *)
(*Setup*)


(* ::Subsection::Closed:: *)
(*Initial functions*)


(* ::Input:: *)
(*ClearAll[timing]*)
(*SetAttributes[timing,{HoldFirst,SequenceHold}]*)
(*timing[expr_]:=Module[{res=AbsoluteTiming[expr]},Print["Timing (line "<>ToString@$Line<>")  :  "<>ToString@First@res];res[[2]] ]*)
(**)
(*ClearAll[timingc]*)
(*SetAttributes[timingc,Attributes@timingc]*)
(*timingc[args___]:=Function[,timing@TimeConstrained[#,args],{HoldAll,SequenceHold}]*)


(* ::Input:: *)
(*ClearAll[reapPrint]*)
(*SetAttributes[reapPrint,HoldAll]*)
(*Options[reapPrint]={"tag"->_};*)
(*(* Like Reap, but prints Last@*Reap and returns First@*Reap *)*)
(**)
(*reapPrint[postFunc_:Identity,opts:OptionsPattern[] ]:=*)
(*With[{tag=OptionValue@"tag"},*)
(*Function[,*)
(*With[{harvest=Reap[#,tag]},Print[postFunc@Last@harvest];First@harvest]*)
(*,*)
(*HoldAll]*)
(*]*)
(**)
(*ClearAll[reapCount]*)
(*SetAttributes[reapCount,HoldAll]*)
(*reapCount[expr_,args___]:=reapPrint[("The count is (Line "<>ToString@$Line<>") : "<>ToString@#&)@*Length@*Flatten,args][expr]*)
(**)
(*ClearAll[echoFunc]*)
(*SetAttributes[echoFunc,HoldAll]*)
(*echoFunc[func_]:=*)
(*Function[,Print["Function result (Line ",$Line,") : ",func@#];#,HoldAll]*)
(**)
(*ClearAll[echoLength]*)
(*echoLength[expr_]:=(Print["Length (Line ",$Line,") : ",Length@expr];expr)*)
(**)


(* ::Input:: *)
(*ClearAll[replaceAllDepthFirst]*)
(*Options[replaceAllDepthFirst]={"positionOptions"->Automatic};*)
(*replaceAllDepthFirst[expr_,rule:(lhs_:>rhs_)|(lhs_->rhs_),opts:OptionsPattern[] ]:=*)
(*With[{popts= Sequence@@List@If[OptionValue@"positionOptions"===Automatic,Nothing,OptionValue@"positionOptions"] },*)
(*MapAt[Replace[rule],expr,Position[expr,lhs,popts]]*)
(*]*)
(*replaceAllDepthFirst[rule_][expr_]:=replaceAllDepthFirst[expr,rule]*)
(**)


(* ::Input:: *)
(*Nest[{a}->{a,{a}},{a},3]*)


(* ::Input:: *)
(*Nest[ReplaceAll[{a}->{a,{a}}],{a},3]*)
(*Module[{n=0},*)
(*replaceAllDepthFirst[%,ll:{___,a,___}:>Append[ll,++n] ]*)
(*]*)


(* ::Input:: *)
(*ClearAll[unique]*)
(*unique[args___]:=Unique[args]*)
(*unique[s_String]:=Unique[Symbol[s]]*)


(* ::Input:: *)
(*ClearAll[stringMatchQ]*)
(*(* wrapper for StringMatchQ.  Remains unevaluated if applied to a non-string *)*)
(*stringMatchQ[expr_String,patt_,args___]:=StringMatchQ[expr,patt,args]*)
(*stringMatchQ[patt_][expr_]:=stringMatchQ[expr,patt]*)


(* ::Input:: *)
(*ClearAll[reverseAssoc]*)
(*(* swaps the keys and values in an association *)*)
(*(* well-defined only for associations with unique values *)reverseAssoc[assoc_Association]:=Association@(Reverse/@Normal@assoc)*)
(**)
(*ClearAll[reverseAssocList]*)
(*(* Assumes the values of an association are lists *)*)
(*(* swaps keys and values, associating each key to each *element* of each value *)*)
(*reverseAssocList[assoc_Association]:=Association@Flatten@(Function[{list,val},#->val&/@list]@@@(Transpose@{Values@assoc,Keys@assoc}))*)
(**)
(*ClearAll[selectAssoc]*)
(*selectAssoc[assoc_Association,f_]:=Association@(Select[Normal@assoc,f@@Reverse[#]&])*)
(*selectAssoc[assoc_Association,f_,"heads"|True]:=Association@(Select[Normal@assoc,f@@Append[Reverse@(List@@#),Head@#]&])*)
(*selectAssoc[assoc_Association,f_,False]:=selectAssoc[assoc,f]*)
(*selectAssoc[f_][assoc_]:=selectAssoc[assoc,f]*)
(**)
(*ClearAll[casesAssoc]*)
(*(* To use levelSpec, a key pattern must be specified.  To get around this, use Cases instead. *)*)
(*casesAssoc[assoc_Association,kPatt_:_,vPatt_,args___]:=*)
(*(*Cases[Replace[rule:(ruleHead_?ruleHeadQ)[key:kPatt,val:vPatt]\[RuleDelayed]key~ruleHead~rule]/@assoc,HoldPattern[ruleHead_?ruleHeadQ[kPatt,vPatt]],args]*)*)
(*(*Association@Cases[Normal@assoc,HoldPattern@@{kPatt\[Rule]vPatt|kPatt\[RuleDelayed]vPatt},args]*)*)
(*selectAssoc[assoc,MatchQ[kPatt~_?ruleHeadQ~vPatt][#2~#3~#1(*Module[{temp=#2~#3~#1},Print[temp];temp]*)]&,"heads"]*)
(**)
(*casesAssoc[(h:Except[_Association]),args___][expr_Association]:=casesAssoc[expr,h,args]*)
(**)
(*ClearAll[casesAssocRule]*)
(*casesAssocRule[assoc_Association,myRule:(lhs:kPatt_~arh_?ruleHeadQ~vPatt_)~rh_?ruleHeadQ~rhs_]:=Replace[myRule]/@Normal@casesAssoc[assoc,kPatt,vPatt]//Association*)
(**)
(*casesAssocRule[(h:Except[_Association]),args___][expr_Association]:=casesAssocRule[expr,h,args]*)
(**)
(*ClearAll[intersectionAssoc]*)
(*(* elements of an association are considered the same if their key, value pairs are the same *)*)
(*intersectionAssoc[h_[assoc__Association] ]:=Intersection@@(Normal/@{assoc})//Apply[h]*)
(**)
(*(* throws away key information of surrounding association *)*)
(*intersectionAssoc[bigAssoc_Association/;And@@MatchQ[_Association]/@Values@bigAssoc]:=intersectionAssoc[Values@bigAssoc]*)
(**)


(* ::Input:: *)
(*ClearAll[intersection,complement]*)
(*(* Like built-in functions, but do not sort returned list.  Maintains the sorting of the first argument, up to deletions. *)*)
(**)
(*intersection[A_,B_]/;Length@A>0(*&&Length@B>0*):=Select[A,MemberQ[B,#]&]*)
(*intersection[A_,B_,CC_,args___]:=intersection[intersection[A,B],CC,args]*)
(*intersection[{},___]:={}*)
(**)
(*complement[A_,B_]/;Length@A>0(*&&Length@B>0*):=Select[A,!MemberQ[B,#]&]*)
(*complement[A_,B_,CC_,args___]:=complement[A,Union[B,CC],args]*)
(*complement[{},___]:={}*)


(* ::Input:: *)
(*(*{<|"a"\[Rule]1,"b"\[Rule]2|>,<|"a"\[Rule]1,"b"\[Rule]3|>}*)
(*Association[MapIndexed[#2\[Rule]#1&]@%]*)
(*MatchQ[_Association]/@%*)*)


(* ::Input:: *)
(*(*{<|"a"\[Rule]1,"b"\[Rule]2|>,<|"a"\[Rule]1,"b"\[Rule]3|>}*)
(*intersectionAssoc@%*)*)


(* ::Input:: *)
(*(*<|{\[Mu],"u"}\[Rule]1,{\[Mu],"l"}\[Rule]1,{\[Nu],"u"}\[Rule]1|>*)
(*casesAssocRule[%,({a_,"u"}\[Rule]b_)\[RuleDelayed](a\[Rule]b)]*)*)


(* ::Input:: *)
(*(*<|"a"\[Rule]1,"b"\[Rule]1|>*)
(*casesAssoc[%,"a",_]*)
(*%%;*)
(*(*Normal@%*)
(*Cases[%,HoldPattern@@{_\[Rule]1}]*)*)*)


(* ::Input:: *)
(*(*<|{\[Mu],"u"}\[Rule]1,{\[Mu],"l"}\[Rule]1,{\[Nu],"u"}\[Rule]1|>*)
(*Normal@%*)
(*Replace[rule:(ruleHead_?ruleHeadQ)[key:_,val:_]\[RuleDelayed]key~ruleHead~rule]/@%*)*)


(* ::Input:: *)
(*(*rule:(ruleHead_?ruleHeadQ)[key:kPatt,val:vPatt]\[RuleDelayed]key~ruleHead~rule*)
(*Head@%*)
(*Replace[%%][{\[Mu],"u"}\[Rule]1]*)
(**)*)


(* ::Input:: *)
(*ClearAll[hasAttributeQ]*)
(*(* Check whether head has a certain attribute *)*)
(*hasAttributeQ[head_,attribute_]:=MemberQ[Attributes[head],attribute]*)
(*hasAttributeQ[attribute_][head_]:=hasAttributeQ[head,attribute]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*ClearAll[mapDenom]*)
(*mapDenom[ff_,expr_]:=Numerator@expr/ff@Denominator@expr*)
(*mapDenom[ff_][expr_]:=mapDenom[ff,expr]*)
(**)
(*ClearAll[mapNum]*)
(*mapNum[ff_,expr_]:=ff@Numerator@expr/Denominator@expr*)
(*mapNum[ff_][expr_]:=mapNum[ff,expr]*)
(**)
(*ClearAll[mapNumDenom]*)
(*mapNumDenom[ff_,expr_]:=ff@Numerator@expr/ff@Denominator@expr*)
(*mapNumDenom[ff_][expr_]:=mapNumDenom[ff,expr]*)
(**)
(**)
(**)
(*ClearAll[repDenom]*)
(*repDenom[rules_,args___]:=mapDenom[ReplaceAll@rules,args]*)
(**)
(*ClearAll[repNum]*)
(*repNum[rules_,args___]:=mapNum[ReplaceAll@rules,args]*)
(**)
(*ClearAll[repNumDenom]*)
(*repNumDenom[rules_,args___]:=mapNumDenom[ReplaceAll@rules,args]*)


(* ::Input:: *)
(*ClearAll[tableForm]*)
(*(* Like TableForm, but actually implemented like Column@*Row *)*)
(**)
(*(* Needs a lot of work *)*)
(**)
(*(* eventual goal *)*)
(*(*tableForm[expr_,cOpts_List:{},rOpts_List:{},opts:OptionsPattern[] ]:=Column[ Row[#,Sequence@@rOpts, FilterRules[opts,Options@Row]]&/@expr,Sequence@@cOpts, FilterRules[opts,Options@Column] ]*)*)
(**)
(*(* quick and dirty *)*)
(*tableForm[expr_, args___]:=Column[  Row[#]&/@expr, ItemSize->Full]*)
(**)
(*ClearAll[matrixForm]*)
(*matrixForm[expr_,args___]:=MatrixForm@{{tableForm[expr,args]}}*)
(**)


(* ::Input:: *)
(*(* Degenerate logical value *)*)
(*ClearAll[none]*)
(**)
(*{none||False,none && True}*)
(**)
(*(*none/:none||False:=False*)
(*none/:none&&True:=True*)*)
(*none/:And[a___,none,b___]:=And[a,b]*)
(*none/:Or[a___,none,b___]:=Or[a,b]*)
(**)
(*{none||False,none && True}*)
(**)


(* ::Input:: *)
(**)


(* ::Subsection::Closed:: *)
(*Definitions*)


(* ::Subsubsection:: *)
(*ruleHeadQ*)


(* ::Input:: *)
(*ClearAll[ruleHeadQ,ruleHeadExceptionQ]*)
(*ruleHeadQ[Rule|RuleDelayed]:=True*)
(*ruleHeadQ[expr_?(Not@*ruleHeadExceptionQ)]:=False*)
(**)
(*ruleHeadExceptionQ[___]:=False*)


(* ::Input:: *)
(*reverseAssocList[<|"a"->{1,3,5},"b"->{2,4},"c"->{-1}|>]*)


(* ::Subsubsection:: *)
(*Indices*)


(* ::Input:: *)
(*ClearAll[declareIndexHead]*)
(*ClearAll[indexHeadQ]*)
(**)
(*ClearAll[getInds,getIndsPos,getIndsSpec]*)
(**)
(*getIndsSpec[x:head_?indexHeadQ[___]]:=(*Transpose@{#,fullIndsPos[getIndsPos@x,Length@#]}&@getInds@x*)Transpose@{getInds@x,getIndsPos@x}*)
(**)
(**)
(*(* declereIndexHead *)*)
(**)
(*declareIndexHead[head_,indsSlot_?NumberQ]:=declareIndexHead[head,Part[#,indsSlot]&,Part[#,indsSlot+1]&]*)
(*declareIndexHead[head_,indsSlot_?NumberQ,indsPosSlot_?NumberQ]:=declareIndexHead[head,Part[#,indsSlot]&,Part[#,indsPosSlot]&]*)
(*declareIndexHead[head_,indsSlot_?NumberQ,posSpec_]:=declareIndexHead[head,Part[#,indsSlot]&,posSpec]*)
(**)
(*declareIndexHead[head_,{indsSlot_?NumberQ}]:=declareIndexHead[head,{indsSlot},indsSlot+1]*)
(*(*declareIndexHead[head_,{indsSlot_?NumberQ},posSlot_?NumberQ]:=declareIndexHead[head,Apply[List]@*(Part[#,indsSlot]&),Part[#,posSlot]&]*)*)
(*declareIndexHead[head_,{indsSlot_?NumberQ},posFunc_]:=declareIndexHead[head,(*Apply[List]@**)(Part[#,indsSlot]&),posFunc]*)
(**)
(*declareIndexHead[head_,indsSpec_,pos_String]:=declareIndexHead[head,indsSpec,pos&]*)
(*declareIndexHead[head_,indsSpec_,posSlot_?NumberQ]:=declareIndexHead[head,indsSpec,Part[#,posSlot]&]*)
(**)
(*declareIndexHead[head_,getIndsFunc_,getIndsPosFunc_]:=(indexHeadQ[head]:=True; getInds[x_head]:=Module[{temp=getIndsFunc@x},If[!Head@temp===List,List@temp,temp] ];*)
(*(* getIndsPos -- Always returns a full index postion specification -- list of characters *)getIndsPos[x_head]:=fullIndsPos[getIndsPosFunc@x,Length@getInds@x])*)
(**)
(**)
(**)
(**)


(* ::Input:: *)
(*ClearAll[fullIndsPos]*)
(*(* Always returns a list of characters *)*)
(*fullIndsPos[spec0_String,length_?IntegerQ]/;Length@spec0>length:=Characters@StringTake[spec0,length]*)
(*fullIndsPos[spec0_String,length_?IntegerQ]/;Length@spec0<length:=Characters@StringPadRight[spec0,length,StringTake[spec0,-1]]*)
(*fullIndsPos[spec0_String,length_?IntegerQ]/;Length@spec0==length:=Characters@spec0*)
(**)
(*(* TODO -- make less hacky *)*)
(*fullIndsPos[spec0:{__String},length_?IntegerQ]:=fullIndsPos[StringJoin@@spec0,length]*)
(**)
(*ClearAll[bareIndsPosQ]*)
(*bareIndsPosQ[spec_String]:=stringMatchQ["u"|"l"|(___~~("ul"|"lu"))]@spec*)
(*(*bareIndsPosQ[{s__String}]:=bareIndsPosQ@StringJoin[s]*)*)
(**)
(*ClearAll[bareIndsPos]*)
(*bareIndsPos[spec0_String?bareIndsPosQ]:=spec0*)
(*bareIndsPos[spec0_String?(Not@*bareIndsPosQ)(*(StringMatchQ@RegularExpression["[ul]+"]) *)]:=(*StringReplace[spec0,Shortest[start___]~~(endChar:"u"|"l")..\[RuleDelayed]start<>endChar]*)*)
(*Characters@spec0 ~Replace~ ({Shortest[start___],(endChar:"u"|"l")..}:>{start,endChar})//Apply@StringJoin*)
(*bareIndsPos[{s__String}]:=Module[{str=StringJoin@s},bareIndsPos@str/;bareIndsPosQ@str]*)


(* ::Input:: *)
(*(*bareIndsPosQ/@{"ul","ull","ullu","ulluu"}*)*)


(* ::Input:: *)
(*(*bareIndsPos/@{"ul","ull","ullu","ulluu"}*)*)


(* ::Input:: *)
(*ClearAll[inds]*)
(**)
(**)
(**)
(*inds[A_,indList_,"lower",args___]:=inds[A,indList,"l",args]*)
(*inds[A_,indList_,"upper",args___]:=inds[A,indList,"u",args]*)
(**)
(*inds[A_,indList_,indsSpec0_?(Not@*bareIndsPosQ),args___]:=inds[A,indList,bareIndsPos@indsSpec0,args]*)
(**)
(*Format[inds[f_Function,indList_,pos_String]]:=Row@{"(",#,")"}&@(f@@indList)*)
(*Format[inds[f_wrapped,indList_,pos_String]]:=Row@{"(",#,")"}&@(f@@indList)*)
(**)
(*Format[inds[A_,indList_List,"l"]]:=Subscript[A,Row[indList]]*)
(*Format[inds[A_,indList_List,"u"]]:=Superscript[A,Row[indList]]*)
(*Format[inds[A_,indList_List,indsPos_?(stringMatchQ@RegularExpression["[ul]+"])]]:=*)
(*Module[{pos=(Characters@StringPadRight[indsPos,Length@indList,StringTake[indsPos,-1]])[[;;Length@indList]]},*)
(*Module[{uDisp= Transpose@{indList,pos}/.{{i_,"l"}:>Invisible[i],{i_,"u"}:>i},lDisp= Transpose@{indList,pos}/.{{i_,"u"}:>Invisible[i],{i_,"l"}:>i} }, Subsuperscript[A,Row@lDisp,Row@uDisp]]*)
(*]*)
(**)
(*ClearAll[indl,indu]*)
(*indl[ind_,args___][expr_]:=inds[expr,{ind},"l",args]*)
(*indu[ind_,args___][expr_]:=inds[expr,{ind},"u",args]*)
(**)
(*indl[ind_List,args___][expr_]:=inds[expr,ind,"l",args]*)
(*indu[ind_List,args___][expr_]:=inds[expr,ind,"u",args]*)
(**)
(*declareIndexHead[inds,2]*)
(*declareIndexHead[indl,(*Apply[List]@**)First,"l"]*)
(*declareIndexHead[indl,(*Apply[List]@**)First,"u"]*)


(* ::Input:: *)
(*(*indl[\[Alpha]]*)
(*getInds@%*)
(*getIndsPos@%%*)
(*getIndsSpec@%%%*)*)


(* ::Input:: *)
(*(*inds[Function@inds[T,{##},"l"],{\[Mu],\[Nu]},"l"]*)
(*%//FullForm*)
(*%%;*)
(*%/.inds[f_Function,indList_,pos_String]\[Rule]post*)*)


(* ::Input:: *)
(*(*inds[T,{\[Mu],\[Nu]},"ll"]*)
(*getInds@%*)
(*%%;*)
(*getIndsPos@%*)
(*%%;*)
(*getIndsSpec@%*)*)


(* ::Input:: *)
(*(*inds[A,{\[Mu]},"l"]*)
(*getInds@%*)
(*%%;*)
(*getIndsPos@%*)
(*%%;*)
(*getIndsSpec@%*)*)


(* ::Input:: *)
(*(*(* Test *)*)
(*inds[A,{\[Mu],\[Nu]},"lower"]*)
(*inds[B,{\[Mu],\[Nu]},"upper"]*)
(*FullForm/@{%,%%}*)
(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Nu]},"ull"]*)*)


(* ::Input:: *)
(*ClearAll[extractIndsSpec, extractInds]*)
(*extractIndsSpec[expr_]:=Module[{code=unique@"extractIndsSpec",dummy},Flatten[#,2]&@Last@Reap[expr/.x:h_?indexHeadQ[___]/;Sow[getIndsSpec@x,code]&&False->dummy,code] ]*)
(*(*ClearAll[extractIndsSpecMod]*)
(*extractIndsSpecMod[expr_]:=Module[{code=ToString@unique@"extractIndsSpecMod",dummy},Flatten[#,0]&@Last@Reap[expr/.x:h_?indexHeadQ[___]/;Sow@@@getIndsSpec@x&&False\[Rule]dummy] ]*)*)
(**)
(*extractIndsSpec[expr_Plus|expr_List]:=With[{numberedAssoc=Merge[Max]@(reverseAssocList/@countIndsSpec/@(List@@expr))},Table[#1,#2]&@@@(Normal@numberedAssoc)//Flatten[#,1]&]*)
(**)
(*extractInds[expr_]:=First/@extractIndsSpec[expr]*)
(**)
(*ClearAll[countIndsSpec,countInds]*)
(*countIndsSpec[expr_]:=GatherBy[extractIndsSpec@expr,Identity]//GroupBy[#,Length->First]&*)
(*countInds[expr_]:=GatherBy[extractInds@expr,Identity]//GroupBy[#,Length->First]&*)
(**)
(*ClearAll[extractDummyInds]*)
(*extractDummyInds[expr_]:=Apply[Intersection][Values@(Keys/@#)&@(casesAssoc[1]/@Function[x,#->casesAssocRule[x,({ind_,#}->n_):>(ind->n)]&/@{"u","l"}//Association]@(*Merge[Max]@*)(reverseAssocList@countIndsSpec[expr]))]*)
(**)
(*ClearAll[extractNonDummyIndsSpec]*)
(*extractNonDummyIndsSpec[expr_]:=Replace[{#,__}->Nothing&/@extractDummyInds@expr]/@extractIndsSpec[expr]*)
(**)
(*ClearAll[extractNonDummyInds]*)
(*extractNonDummyInds[expr_]:=DeleteDuplicates@(First/@extractNonDummyIndsSpec[expr])*)
(**)
(*ClearAll[replaceDummyInds]*)
(**)
(*(* Need to pass on forbidden arguments when calling on _Plus *)*)
(*(*replaceDummyInds[expr_Plus,args___]:=replaceDummyInds[#,args]&/@expr*)
(*replaceDummyInds[expr_List,args___]:=replaceDummyInds[#,args]&/@expr*)*)
(*Options[replaceDummyInds]={"forbiddenInds"->{},"newIndList"->{\[Mu],\[Nu],\[Rho],\[Sigma],\[Alpha],\[Beta],\[Gamma],\[Delta]},"ignoreInds"->{}};*)
(*(*replaceDummyInds[expr_,newIndList0_:{\[Mu],\[Nu],\[Rho],\[Sigma],\[Alpha],\[Beta],\[Gamma],\[Delta]},forbiddenIndList0_:{}]:=*)*)
(*replaceDummyInds[expr_,OptionsPattern[] ]:=*)
(*Module[{oldInds=DeleteDuplicates@extractInds@expr,indsToReplace=complement[extractDummyInds@expr,OptionValue@"ignoreInds"]},*)
(*Module[{nonDummyInds=complement[oldInds,indsToReplace]},*)
(*Module[{forbiddenIndList=nonDummyInds~Union~(*forbiddenIndList0*)OptionValue@"forbiddenInds"},*)
(*Module[{newIndList=complement[(*newIndList0*)OptionValue@"newIndList",forbiddenIndList],n=0},*)
(*While[Length@newIndList<Length@indsToReplace,newIndList=Join[newIndList,complement[Symbol[ToString@#1<>ToString@n]&/@(*newIndList0*)OptionValue@"newIndList",forbiddenIndList]];n++];newIndList=newIndList[[;;Length@indsToReplace]];*)
(*expr/.Rule@@@Transpose@{indsToReplace,newIndList}] *)
(*] ] ] *)
(**)
(**)
(*(* rather than searching for dummy inds, instead replace only those specified *)*)
(*(* Needs some work *)*)
(*(*replaceDummyInds[expr_,indsToReplace0_List,OptionsPattern[] ]:=*)
(*Module[{oldInds=DeleteDuplicates@extractInds@expr,indsToReplace=complement[indsToReplace0,OptionValue@"ignoreInds"]},*)
(*Module[{nonDummyInds00=complement[oldInds,indsToReplace]},*)
(*Module[{forbiddenIndList=nonDummyInds~Union~(*forbiddenIndList0*)OptionValue@"forbiddenInds"},*)
(*Module[{newIndList=complement[(*newIndList0*)OptionValue@"newIndList",forbiddenIndList],n=0},*)
(*While[Length@newIndList<Length@indsToReplace,newIndList=Join[newIndList,complement[Symbol[ToString@#1<>ToString@n]&/@(*newIndList0*)OptionValue@"newIndList",forbiddenIndList]];n++];newIndList=newIndList\[LeftDoubleBracket];;Length@indsToReplace\[RightDoubleBracket];*)
(*expr/.Rule@@@Transpose@{indsToReplace,newIndList}] *)
(*] ] ] *)*)
(**)


(* ::Text:: *)
(*Replacing indices*)
(**)
(*	Index groups:*)
(*	*)
(*	* ignoreInds*)
(*	* forbiddenInds*)
(*	* newIndList (aka baseDummyInds)*)
(*	*)
(*	* oldInds*)
(*	* indsToReplace = Select[oldInds, testQ] - ignoreInds*)
(*	* *)


(* ::Input:: *)
(*ClearAll[padRight]*)
(*padRight*)


(* ::Input:: *)
(*Module[{x},PadRight[{},10,x]/.x:>RandomReal[]]*)


(* ::Input:: *)
(*indl[a]@k indl[b]@k inds[T,{a,b},"u"]*)
(*replaceDummyInds@%*)
(*%%;*)
(*replaceDummyInds[%,"ignoreInds"->{a}]*)


(* ::Input:: *)
(*indl[#]@a indu[#]@b&/@{\[Alpha],\[Beta],\[Gamma],\[Delta]}//Apply[Times];*)
(*%//replaceDummyInds*)
(*%%;*)
(*replaceDummyInds[%,"forbiddenInds"->{\[Mu]}]*)


(* ::Input:: *)
(*(* Function for simplifying expressions with dummy indices *)*)
(**)
(*ClearAll[dummyIndexGroups]*)
(*(* Collect a sum (or list) of terms into groups which are the same under identification of all dummy indices *)*)
(*dummyIndexGroups[expr_List]:=*)
(*Module[{z},*)
(*GroupBy[expr,Replace[a_?NumberQ*b_:>b]@*(ReplaceAll[#->z&/@extractDummyInds@expr])]*)
(*]*)
(*dummyIndexGroups[expr_Plus]:=dummyIndexGroups[List@@expr]*)


(* ::Input:: *)
(*(*inds[A,{\[Mu],\[Nu]},"ul"]*)
(*extractIndsSpec@%*)
(*inds[A,{\[Mu],\[Nu]},"ul"]f@inds[B,{\[Mu]0,\[Nu]0},"ul"]*)
(*extractIndsSpec@%*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]+inds[\[CapitalGamma],{\[Beta],\[Mu],\[Beta]},"ull"]*)
(*%//replaceDummyInds*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]+inds[\[CapitalGamma],{\[Beta],\[Mu],\[Beta]},"ull"]*)
(*extractDummyInds@%*)
(*%%;*)
(*extractNonDummyIndsSpec@%*)
(*%%;*)
(*extractNonDummyInds@%*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]+inds[\[CapitalGamma],{\[Beta],\[Mu],\[Beta]},"ull"]*)
(*extractIndsSpec@%*)
(*%%*)
(*countIndsSpec@%*)
(*%%*)
(*extractInds@%*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]*)
(*countIndsSpec@%*)
(*%%;*)
(*extractIndsSpec@%*)
(*GatherBy[%,Identity]*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]+inds[\[CapitalGamma],{\[Beta],\[Mu],\[Beta]},"ull"]*)
(*extractIndsSpec@%*)
(**)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]+inds[\[CapitalGamma],{\[Beta],\[Mu],\[Beta]},"ull"]*)
(*extractIndsSpec/@List@@%*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]*)
(*extractDummyInds@%*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]*)
(*countIndsSpec@%*)
(*reverseAssocList@%*)
(*(*Merge[Max]@%*)*)
(*#->casesAssocRule[%,({ind_,#}\[Rule]n_)\[RuleDelayed](ind\[Rule]n)]&/@{"u","l"}//Association*)
(*casesAssoc[1]/@%*)
(*Values@(Keys/@%)*)
(*Intersection@@%*)*)


(* ::Input:: *)
(*(*extractDummyInds@inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Alpha]},"ull"]*)*)


(* ::Input:: *)
(*(*inds[\[CapitalGamma],{\[Alpha],\[Mu],\[Nu]},"ull"]*)
(*%//countIndsSpec*)
(*%%//countInds*)*)


(* ::Subsubsection:: *)
(*Declare heads and tests*)


(* ::Input:: *)
(*(*ClearAll[rhsCondition]*)
(*SetAttributes[rhsCondition,HoldAll]*)
(*rhsCondition/:SetDelayed[lhs_,rhsCondition[condition_,body_] ]:=SetDelayed[lhs,Module[{trueQ=condition},If[trueQ,body]/;trueQ]]*)*)


(* ::Input:: *)
(*ClearAll[foldHeld]*)
(*SetAttributes[foldHeld,HoldAll]*)
(*foldHeld[f_,base_,{arg_}]:=f[base,arg]*)
(*foldHeld[f_,base_,{nextArg_,laterArgs__}]:=foldHeld[f,f[base,nextArg],{laterArgs}]*)
(**)


(* ::Input:: *)
(*ClearAll[declareSubsetHeadTest]*)
(*(* All subs are sups *)*)
(*declareSubsetHeadTest[subQ_][supQ_]:=(supQ[args___]/;subQ[args]:=True(*;subQ[args___]/;!supQ[args]:=False*))*)
(**)
(*ClearAll[declareHeadTest,headQ]*)
(*declareHeadTest[headTestQ_]:=(*Null*)declareSubsetHeadTest[headTestQ][headQ]*)
(**)
(**)
(*ClearAll[declareHead]*)
(*declareHead[head_][testQ_]:=(head/:testQ[head]:=True)*)
(*declareHead[head_][testQ_List]:=declareHead[head][#]&/@testQ(*CompoundExpression@@((head/:#[head]:=True)&/@testQ)*)*)
(**)
(**)
(*ClearAll[derivHeadQ,firstDerivHeadQ,linearHeadQ,leibnizHeadQ,chainRuleHeadQ]*)
(**)
(*declareSubsetHeadTest[firstDerivHeadQ][linearHeadQ]*)
(*declareSubsetHeadTest[firstDerivHeadQ][derivHeadQ]*)
(*declareSubsetHeadTest[firstDerivHeadQ][leibnizHeadQ]*)
(*declareSubsetHeadTest[firstDerivHeadQ][chainRuleHeadQ]*)
(**)
(*(*derivHeadQ[args___]/;firstDerivHeadQ[args]:=True*)
(**)
(*linearHeadQ[args___]/;firstDerivHeadQ[args]:=True*)*)


(* ::Input:: *)
(*(*ClearAll[assume]*)
(*assume[testQ_,truthVal_:True][patt___][code_]:=Module[{assumeContext},testQ[patt,___]/;(True||assumeContext):=truthVal;Module[{result=code},*)
(*DownValues[testQ]=DeleteCases[DownValues[testQ],_?(!FreeQ[#,assumeContext]&)];result ]*)
(*]*)*)
(*ClearAll[assume]*)
(*SetAttributes[assume,{HoldAll(*,SequenceHold*)}]*)
(*Options[assume]={"truthVal"->True,"chainQ"->True};*)
(**)
(*assume[testQ_,list_List/;And@@(Head@#==List&/@list),expr_,OptionsPattern[] ]:=*)
(*((*Print["evalChain: "<>ToString@list];*)(*ReleaseHold@Fold[Hold@assume[testQ,Sequence@@#2,#1]&(*assume[testQ,##]&@(Flatten@{#2,#1}&)*),Hold@expr,list]/;OptionValue["chainQ"]*)*)
(*foldHeld[Function[Null,assume[testQ,Sequence@@#2,#1],{HoldAll}](*assume[testQ,##]&@(Flatten@{#2,#1}&)*),expr,list]/;OptionValue["chainQ"]*)
(*)*)
(**)
(*assume[testQ_,patt___,expr_,OptionsPattern[] ]:=Module[{assumeContext},testQ[patt,___]/;(True||assumeContext)=OptionValue[assume,"truthVal"];(*Print["evalNormal: "<>ToString@{patt}];*)Module[{result=expr},*)
(*Scan[(#[testQ]=DeleteCases[#[testQ],_?(!FreeQ[#,assumeContext]&)])&,{DownValues,OwnValues}];result ]*)
(*]*)
(**)
(**)
(*ClearAll[constantQ]*)
(*Options[constantQ]={"threadIndsQ"->True};*)
(*constantQ[x_?NumberQ,___]:=True*)
(*(*constantQ[inds[expr_,___],args___,OptionsPattern[] ]:=constantQ[expr,args]/;OptionValue["threadIndsQ"]*)*)
(*constantQ[inds[expr_,___],args___,OptionsPattern[] ]:=True/;OptionValue["threadIndsQ"]&&constantQ[expr,args]*)
(**)


(* ::Input:: *)
(*ClearAll[rule]*)
(*rule[list_List]:=Flatten[rule/@list]*)
(**)
(*rule[leibnizHead]={h_?leibnizHeadQ[factors_Times,args___]:>Module[{i},Plus@@Table[MapAt[h[#,args]&,factors,i],{i,Length@factors}]],*)
(*h_?leibnizHeadQ[Power[a_,b_],args___ ]:> b a^(b-1) h[a,args]+a^b Log[a] h[b,args],h_?leibnizHeadQ[var_,var_,args___]:>1}*)
(*rule[linearHead]=h_?linearHeadQ[sum_Plus,args___]:>(h[#,args]&/@sum)*)
(*rule[derivHead]={p:h_?derivHeadQ[a_,var_,args___]/;constantQ[a,var]||If[indexHeadQ@h===True,constantQ[a,var,getInds@p],False]->0,p:h_?derivHeadQ[a_*x_,var_,args___]/;constantQ[a,var]||If[indexHeadQ@h===True,constantQ[a,var,getInds@p],False]:>a h[x,var,args]}*)
(*rule[chainRuleHead]={h_?chainRuleHeadQ[f_?(hasAttributeQ@NumericFunction)[x_],var_,args___]:>f'[x]h[x,var,args]}*)
(*rule[firstDerivHead]:=rule@{leibnizHead,linearHead,derivHead,chainRuleHead}*)
(**)
(**)
(*rule[kdelta]={*)
(*(*inds[kdelta,{i_,j_},"lu"]*expr:(a_*h_?indexHeadQ[hargs___])/;(!FreeQ[expr,i]||!FreeQ[expr,j])&&(MemberQ[countInds[expr][1],])\[RuleDelayed]post*)*)
(*inds[kdelta,{i_,j_},___]*expr_/;!FreeQ[expr,i]:>(expr/.i->j),*)
(*inds[kdelta,{j_,i_},___]*expr_/;!FreeQ[expr,i]:>(expr/.i->j)*)
(*}*)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Derivatives*)


(* ::Input:: *)
(*ClearAll[deriv]*)
(*(*deriv/:derivHeadQ[deriv]:=True*)*)
(*declareHead[deriv][firstDerivHeadQ]*)
(*Format[deriv[expr_,var_]]:=Row@{Subscript["\[PartialD]",var],"[",expr,"]"}*)
(**)
(**)
(**)
(*ClearAll[indexDeriv]*)
(*declareHead[indexDeriv][firstDerivHeadQ]*)
(**)
(*indexDeriv[inds[var_,{\[Mu]_},"l"],var_,\[Nu]_,"u"]:=inds[kdelta,{\[Mu],\[Nu]},"lu"]*)
(**)
(*(*Format[indexDeriv[expr_,var_,ind_,"l"]]:=Row@{Subsuperscript["\[PartialD]",ind,Row@{"(",var,")"}],"[",expr,"]"}*)
(*Format[indexDeriv[expr_,var_,ind_,"u"]]:=Row@{Subsuperscript["\[PartialD]",Row@{"(",var,")"},ind],"[",expr,"]"}*)*)
(**)
(*Format[indexDeriv[expr_,var_,ind_,"l"]]:=Row@{Subscript[Overscript["\[PartialD]",Row@{"(",var,")"}],ind],"[",expr,"]"}*)
(*Format[indexDeriv[expr_,var_,ind_,"u"]]:=Row@{Superscript[Overscript["\[PartialD]",Row@{"(",var,")"}],ind],"[",expr,"]"}*)
(**)
(**)
(*ClearAll[pdl,pdu]*)
(*pdl[ind_]:=pdl[x,ind]*)
(*pdu[ind_]:=pdu[x,ind]*)
(*pdl[var_,ind_][expr_]:=indexDeriv[expr,var,ind,"l"]*)
(*pdu[var_,ind_][expr_]:=indexDeriv[expr,var,ind,"u"]*)
(**)
(*Format[pdl[var_,ind_]]:=Subsuperscript["\[PartialD]",ind,Row[{"(",var,")"}]]*)
(*Format[pdu[var_,ind_]]:=Subsuperscript["\[PartialD]",Row[{"(",var,")"}],ind]*)
(**)
(*declareIndexHead[indexDeriv,{3}]*)
(*declareIndexHead[pdl,{2},"l"]*)
(*declareIndexHead[pdu,{2},"u"]*)
(**)
(*ClearAll[covarDeriv]*)
(*declareHead[covarDeriv][firstDerivHeadQ]*)
(**)
(*Format[covarDeriv[expr_,ind_,"l"]]:=Row@{Subscript["\[Del]",ind],"[",expr,"]"}*)
(*Format[covarDeriv[expr_,ind_,"u"]]:=Row@{Superscript["\[Del]",ind],"[",expr,"]"}*)
(**)
(*Format[covarDeriv[expr_,ind_,"l",disp_]]:=Row@{Subscript[disp,ind],"[",expr,"]"}*)
(*Format[covarDeriv[expr_,ind_,"u",disp_]]:=Row@{Superscript[disp,ind],"[",expr,"]"}*)
(**)
(*ClearAll[cdl,cdu]*)
(*cdl[ind_,args___][expr_]:=covarDeriv[expr,ind,"l",args]*)
(*cdu[ind_,args___][expr_]:=covarDeriv[expr,ind,"u",args]*)
(**)
(*Format[cdl[ind_]]:=Subscript["\[Del]",ind]*)
(*Format[cdu[ind_]]:=Superscript["\[Del]",ind]*)
(**)
(*declareIndexHead[covarDeriv,{2}]*)
(*declareIndexHead[cdl,{1},"l"]*)
(*declareIndexHead[cdu,{1},"u"]*)
(**)


(* ::Input:: *)
(*(*{cdl[a],pdu[b]}*)
(*getIndsSpec/@%*)*)


(* ::Input:: *)
(*(*(* Test *)*)
(*deriv[a*b*c,args]/.rule[leibnizHead]*)
(*deriv[Times]*)*)


(* ::Input:: *)
(*(*(* Test *)*)
(*derivHeadQ[deriv]*)
(*firstDerivHeadQ@deriv*)
(*linearHeadQ@deriv*)*)


(* ::Input:: *)
(*(* TODO *)*)
(*(* *)
(**)
(** assume[...]*)
(*	* Implement by delaring `prop`s (properties)*)
(**)
(** coordinate systems, metrics, changes of basis*)
(**)
(** inds -- automatically format index position spec*)
(**)
(**)*)


(* ::Subsection::Closed:: *)
(*Machinery for calculating Riemann and Ricci tensors*)


(* ::Subsubsection:: *)
(*Build machinery*)


(* ::Input:: *)
(*ClearAll[rule\[CapitalGamma]l]*)
(*rule\[CapitalGamma]l[\[CapitalGamma]_->gg_,ff_:pdl]:=inds[\[CapitalGamma],{\[Gamma]_,\[Mu]_,\[Beta]_},"l"]:>1/2 (ff[\[Mu]]@inds[gg,{\[Gamma],\[Beta]},"l"]+ff[\[Beta]]@inds[gg,{\[Gamma],\[Mu]},"l"]-ff[\[Gamma]]@inds[gg,{\[Mu],\[Beta]},"l"])*)
(*rule\[CapitalGamma]l[\[CapitalGamma]headPatt_:_,ff0_:pdl]:=inds[\[CapitalGamma]headPatt[gg_,ff_:ff0],{\[Gamma]_,\[Mu]_,\[Beta]_},"l"]:>1/2 (ff[\[Mu]]@inds[gg,{\[Gamma],\[Beta]},"l"]+ff[\[Beta]]@inds[gg,{\[Gamma],\[Mu]},"l"]-ff[\[Gamma]]@inds[gg,{\[Mu],\[Beta]},"l"])*)


(* ::Input:: *)
(*ClearAll[ruleCovarDeriv,ruleScalarCovarDeriv]*)
(**)
(*Options[ruleScalarCovarDeriv]={*)
(*"pattern"->Automatic,"defaultPattern"->Except[_?indexHeadQ[___]],*)
(*"derivFunc"->(indexDeriv[#1,x,##2]&)};*)
(*ruleScalarCovarDeriv[ OptionsPattern[] ]:=*)
(*With[{argPatt=If[OptionValue@"pattern"===Automatic,OptionValue@"defaultPattern",OptionValue@"pattern"]},*)
(*covarDeriv[arg:argPatt,cdind_,cdindPos_,___]:>(OptionValue@"derivFunc")[arg,cdind,cdindPos]*)
(*]*)
(**)
(*Options[ruleCovarDeriv]={*)
(*"pattern"->Automatic,"defaultPattern"->(*_?indexHeadQ[___]*)_inds,*)
(*"cdIndPatt"->_,"cdIndPos"->_,"cdTypePatt"->___,"connection"->\[CapitalGamma],"derivFunc"->(*(Function[expr,indexDeriv[expr,x,#1,#2]]&)*)(indexDeriv[#1,x,##2]&),"baseDummyInd"->"\[Gamma]","explicitDummyInds"->{},"metric"->gg,"includeScalars"->True,"scalarArgPatt"->Automatic,"includeNested"->True};*)
(*ruleCovarDeriv[opt:OptionsPattern[] ]:=( *)
(*With[{objPatt=If[OptionValue@"pattern"===Automatic,OptionValue@"defaultPattern",OptionValue@"pattern"]},*)
(*With[{baseRule=*)
(*covarDeriv[cdExpr:objPatt,cdInd:OptionValue@"cdIndPatt",cdIndPos:(OptionValue@"cdIndPos"|_String?(StringMatchQ[OptionValue@"cdIndPos"])),cdType:OptionValue@"cdTypePatt"]:>With[{derivFunc=OptionValue@"derivFunc",dummyInd=If[Length@OptionValue@"explicitDummyInds">=1,First@OptionValue@"explicitDummyInds",unique@OptionValue@"baseDummyInd"],*)
(*connection=OptionValue@"connection",*)
(*cdIndl=If[cdIndPos=="l",cdInd,unique@OptionValue@"baseDummyInd"]*)
(*},With[{prefactor=If[cdIndPos=="l",1,inds[OptionValue@"metric",{cdInd,cdIndl},"u"]]},*)
(*Module[{indsSpec=getIndsSpec@cdExpr//GroupBy[Last->First](*//Merge[{#,AssociationMap[{}&,{"u","l"}]},Apply@Join]&*)},*)
(*derivFunc[cdExpr,cdInd,cdIndPos]+*)
(*Plus@@If[KeyExistsQ["u"]@indsSpec,*)
(*(prefactor*inds[connection,{#,cdIndl,dummyInd},"ul"] (cdExpr/.(#->dummyInd)))&/@indsSpec["u"],0]-Plus@@If[KeyExistsQ["l"]@indsSpec,*)
(*(prefactor*inds[connection,{dummyInd,cdIndl,#},"ul"] (cdExpr/.(#->dummyInd)))&/@indsSpec["l"],0]*)
(*]]]//List*)
(*//If[OptionValue@"includeScalars",Join[#,ruleScalarCovarDeriv@@{If[OptionValue@"scalarArgPatt"===Automatic,Nothing,OptionValue@"scalarArgPatt"],"derivFunc"->OptionValue@"derivFunc"}//List]&,Identity]},*)
(*baseRule*)
(*//If[OptionValue@"includeNested",Join[ruleNestedCovarDeriv[(*(*objPatt,*)"$ruleCovarDeriv"\[Rule]baseRule*)],#],Identity]& (* TODO:  Need to pass ruleCovarDeriv options to ruleNestedCovarDeriv *)*)
(*]]*)
(*)*)
(**)
(**)
(**)


(* ::Input:: *)
(*(* From Mr.Wizard, http://mathematica.stackexchange.com/a/141961/11035 *)*)
(*positionExcept[expr_,test_,opts___]:=With[{p=Position[expr,_?test,opts]},If[p==={{}},{},Position[expr,_,opts]//DeleteCases[Alternatives@@({#,___}&@@@p)]]]*)
(**)
(*Options[mapAllExcept]={Heads->False};*)
(**)
(*mapAllExcept[f_,expr_,test_,OptionsPattern[]]:=MapAt[f,expr,positionExcept[expr,test,Heads->OptionValue[Heads]]]*)


(* ::Input:: *)
(*Remove[ruleWrapInds]*)


(* ::Input:: *)
(*ClearAll[wrapped]*)
(*SetAttributes[wrapped,HoldAllComplete];*)
(*wrapped[ff_Function,argsw___][inds___]:=ff[inds]*)
(**)
(*ClearAll[wrappedQ]*)
(*wrappedQ[___]:=False*)
(*(*wrappedQ[inds[_Function,___]]:=True*)*)
(*wrappedQ[inds[_wrapped,___]]:=True*)
(**)
(**)
(**)
(*ClearAll[ruleWrapInds]*)
(*Options[ruleWrapInds]={"pattern"->Automatic,"defaultPattern"->h1_?indexHeadQ[h2_?indexHeadQ[___],___],"innerObjFunc"->First,"preventRewrappingQ"->True,"wrappedTag"->None,*)
(*"wrapNonNested"->True,"defaultNonNestedPattern"->h1:Except[inds,_?indexHeadQ][___]};*)
(**)
(*ruleWrapInds[opts:OptionsPattern[]]:=( *)
(*With[{objPatt=If[OptionValue@"pattern"===Automatic,*)
(*If[OptionValue@"wrapNonNested",*)
(*OptionValue@"defaultPattern"|OptionValue@"defaultNonNestedPattern",*)
(*OptionValue@"defaultPattern"],*)
(*OptionValue@"pattern"]},*)
(*indObj:objPatt:>*)
(*With[{innerObj=(OptionValue@"innerObjFunc")@indObj},*)
(*Module[{fullIndList=getIndsSpec/@{indObj,If[indexHeadQ@Head@innerObj===True,innerObj,Nothing]}//Flatten[#,1]&},*)
(*inds[wrapped@@{Function@@{indObj/.MapIndexed[#->Slot@@{First@#2}&,First/@fullIndList]},If[#===None||#===Automatic,Nothing,#]&@OptionValue@"wrappedTag"},First/@fullIndList,Last/@fullIndList//StringJoin]*)
(*]]*)
(*]//List*)
(*//If[OptionValue@"preventRewrappingQ",Join[{xx_?wrappedQ:>xx},#]&,Identity]*)
(*)*)
(**)
(**)
(**)
(*ClearAll[ruleUnwrapInds]*)
(*Options[ruleUnwrapInds]={"tagPatt"->___};*)
(**)
(*ruleUnwrapInds[ opts:OptionsPattern[] ]:=*)
(*inds[p:wrapped@@List[ff_Function,tag:OptionValue@"tagPatt"],indList_,___]:>p@@indList*)
(**)
(**)
(**)
(*ClearAll[wrapInds]*)
(*(*Options[wrapInds]={"objPattern"\[Rule]Automatic};*)*)
(*wrapInds[expr_,opts:OptionsPattern[] ]:=(*expr//Replace@ruleWrapInds[(If[#===Automatic,Sequence[],#]&)@OptionValue@"objPattern",FilterRules[opts,Options@ruleWrapInds]]*)*)
(*(*expr//Replace@ruleWrapInds@FilterRules[opts,Options@ruleWrapInds]*)*)
(*mapAllExcept[Replace@ruleWrapInds@FilterRules[Join[{},Flatten@List@opts],Options@ruleWrapInds],expr,wrappedQ]*)


(* ::Input:: *)
(*ClearAll[ruleNestedCovarDeriv]*)
(*Options[ruleNestedCovarDeriv]={"$ruleCovarDeriv"->Automatic}*)
(*ruleNestedCovarDeriv[argPatt_:_covarDeriv(*covarDeriv[_?indexHeadQ[___],___]*),opt:OptionsPattern[] ]:=*)
(*covarDeriv[expr:argPatt,cdargs___]:>*)
(*Module[{wrappedTag},*)
(*covarDeriv[wrapInds[expr,"wrappedTag"->wrappedTag],cdargs]*)
(*/.If[OptionValue@"$ruleCovarDeriv"===Automatic,ruleCovarDeriv[],OptionValue@"$ruleCovarDeriv"]*)
(*//.ruleUnwrapInds["tagPatt"->wrappedTag]*)
(*]//List*)


(* ::Input:: *)
(*cdl[a]@wrapInds@cdl[b]@indl[d]@v*)
(*%/.ruleCovarDeriv[]*)
(*%/.ruleCovarDeriv[]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@indl[d]@v*)
(*%/.ruleCovarDeriv["connection"->\[CapitalGamma]bar]*)
(*%/.ruleCovarDeriv["connection"->\[CapitalGamma]bar]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@indl[d]@v*)
(*%//.ruleCovarDeriv[]*)
(*%%;*)
(*%//.ruleCovarDeriv["connection"->\[CapitalGamma]bar]*)


(* ::Input:: *)
(*(* need this to wrap: *)*)
(*wrapInds[cdl[a]@T]*)
(*wrapInds[cdl[a]@T,"wrapNonNested"->False]*)


(* ::Input:: *)
(*(* test of ruleCovarDeriv *)*)
(*cdl[a]@inds[T,{b,c},#]&/@{"ul","u","l"}*)
(*AssociationMap[ReplaceAll@ruleCovarDeriv["explicitDummyInds"->{"d"}],%]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*cdl[a]@cdl[b]@inds[wrapped[inds[A,{#},"u"]&,"tag"],{c},"u"]*)
(*MapAt[wrapInds,%,1]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@inds[wrapped[inds[A,{#},"u"]&,"tag"],{c},"u"]*)
(*%/.ruleNestedCovarDeriv[]*)
(*%/.ruleCovarDeriv[]*)
(*%/.ruleUnwrapInds[]*)
(*%//Expand;*)
(*%//.rule@firstDerivHead*)


(* ::Input:: *)
(*cdl[b]@xx*)
(*%/.ruleScalarCovarDeriv[]*)
(*%%;*)
(*%/.ruleCovarDeriv[]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@indl[d]@v*)
(*%//.ruleCovarDeriv[]*)
(*%%;*)
(*%//.ruleCovarDeriv["connection"->\[CapitalGamma]bar]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@cdl[c]@xx*)
(*%//.ruleCovarDeriv[]*)
(*%%;*)
(*%//.ruleCovarDeriv["connection"->\[CapitalGamma]bar]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@cdl[c]@xx*)
(*%//.ruleNestedCovarDeriv[]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@cdl[c]@xx*)
(*%//.ruleNestedCovarDeriv[]*)


(* ::Input:: *)
(*cdl[a]@cdl[b]@xx*)
(*%/.ruleNestedCovarDeriv[]*)
(*%/.ruleCovarDeriv[]*)
(*(*%/.ruleCovarDeriv[]*)
(*%/.ruleCovarDeriv["includeScalars"\[Rule]True]*)*)


(* ::Input:: *)
(*Nest[cdl[aa],xx,4]/.aa:>unique@"a"*)
(*(*Nest[ReplaceAll[ruleNestedCovarDeriv[]],%,5]*)*)
(*%//.ruleCovarDeriv[]//timing*)


(* ::Subsubsection:: *)
(*Evaluate Riemann Tensor*)


(* ::Input:: *)
(*(* General expression for riemann tensor with llll indices, calculated from first principles *)*)
(*inds["R",{a,c,b,d},"l"]*)
(*inds[OverBar@"R",{a,c,b,d},"l"]+(cdl[b,cdgbar]@inds[\[CapitalDelta],{a,d,e},"l"]-cdl[d,cdgbar]@inds[\[CapitalDelta],{a,b,e},"l"]-inds[gg,{f,h},"u"](inds[\[CapitalDelta],{h,b,a},"l"]inds[\[CapitalDelta],{f,d,e},"l"]-inds[\[CapitalDelta],{h,d,a},"l"]inds[\[CapitalDelta],{f,b,e},"l"]));*)
(*riemannCalcllll=%/.e->c*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(* expression calculated from first principles *)*)
(*inds["R",{c,d},"l"]*)
(*inds[gg,{a,b},"u"]riemannCalcllll;*)
(*Collect[%,inds[OverBar@"R",___]];*)
(*ricciCalcll=%*)
(*%/.inds[gg,{a_,b_}|{b_,a_},"u"]inds[OverBar@"R",{a_,c_,b_,d_}|{c_,a_,d_,b_},"l"]:>inds[OverBar@"R",{c,d},"l"];*)
(*ricciCalc2ll=%*)


(* ::Input:: *)
(*riemannCalcllll*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*ClearAll[calc]*)
(*Options[calc]={"metric"->gg,"derivFunc"->pdl,"connection"->\[CapitalGamma],"connectionFuncQ"->none,*)
(*"explicitDummyInds"->{f,h},"baseDummyInds"->{},"defaultBaseDummyInd"->"\[Gamma]","checkExplicitDummyIndsQ"->True,*)
(*"backgroundRiemannVanishQ"->False};*)
(*calc["Riemann",indList:{a_,b_,c_,d_},"l",opt:OptionsPattern[] ]:=*)
(*With[{nDummyInds=2},*)
(*Module[{dummyInds=*)
(*Module[{xx},OptionValue@"explicitDummyInds"*)
(*//PadRight[#,nDummyInds,xx/@If[OptionValue@"checkExplicitDummyIndsQ",complement[#,indList]&,Identity]@OptionValue@"baseDummyInds"]&*)
(*//PadRight[#,nDummyInds,xx@OptionValue@"defaultBaseDummyInd"]&*)
(*//ReplaceRepeated[#,xx->unique]&*)
(*]},*)
(*Module[{f,h},{f,h}=dummyInds[[;;nDummyInds]];*)
(*With[{fderiv=OptionValue@"derivFunc",gg=OptionValue@"metric"},*)
(*With[{\[CapitalDelta]=If[OptionValue@"connectionFuncQ"||Head@OptionValue@"connection"===Function,(OptionValue@"connection")@fderiv,OptionValue@"connection"]},*)
(*inds[OverBar["R"],{a,c,b,d},"l"]+*)
(*fderiv[b][inds[\[CapitalDelta],{a,d,c},"l"]]-fderiv[d][inds[\[CapitalDelta],{a,b,c},"l"]]*)
(*-(inds[gg,{f,h},"u"](inds[\[CapitalDelta],{f,d,c},"l"]*inds[\[CapitalDelta],{h,b,a},"l"]*)
(*-inds[\[CapitalDelta],{f,b,c},"l"]*inds[\[CapitalDelta],{h,d,a},"l"]))*)
(*]//If[OptionValue@"derivFunc"===pdl ||OptionValue@ "backgroundRiemannVanishQ",#/.inds[OverBar["R"],___]->0&,Identity]*)
(*]]]]*)
(**)


(* ::Input:: *)
(*calc["Riemann",{\[Alpha],\[Beta],\[Mu],\[Nu]},"l"]*)
(*calc["Riemann",{\[Alpha],\[Beta],\[Mu],\[Nu]},"l","explicitDummyInds"->{\[Gamma],\[Delta]}]*)
(*calc["Riemann",{\[Alpha],\[Beta],\[Mu],\[Nu]},"l","explicitDummyInds"->{\[Gamma]}]*)
(*calc["Riemann",{\[Alpha],\[Beta],\[Mu],\[Nu]},"l","explicitDummyInds"->{\[Gamma],\[Mu]}]*)
(*calc["Riemann",{\[Alpha],\[Beta],\[Mu],\[Nu]},"l","explicitDummyInds"->{\[Gamma],\[Mu]},"checkExplicitDummyIndsQ"->False]*)
(*calc["Riemann",{\[Alpha],\[Beta],\[Mu],\[Nu]},"l","derivFunc"->(cdl[#,cdgbar]&),"connection"->(\[CapitalDelta][hh,#]&)]*)


(* ::Input:: *)
(*calc["Riemann",{a,b,c,d},"l","metric"->gg,"derivFunc"->(cdl[#,cdgbar]&),"connection"->(\[CapitalDelta][hh,#]&)]*)
(*%/.rule\[CapitalGamma]l[\[CapitalDelta]]/.rule@derivHead*)
(*%//.rule@firstDerivHead;*)
(*Expand@%;*)
(*temp=%;*)
(*%/.ruleCovarDeriv[];*)
(*%//Expand;*)
(*%//.rule@firstDerivHead;*)
(*%//Expand;*)
(*%*)


(* ::Subsection::Closed:: *)
(*Example*)


(* ::Input:: *)
(*(*inds[Subscript[R, KS],{\[Mu],\[Nu]},"ul"]*)
(**)
(*1/2pdl[\[Alpha]]@(pdu[\[Mu]]@inds[h,{\[Alpha],\[Nu]},"ul"]+pdl[\[Nu]]@inds[h,{\[Mu],\[Alpha]},"u"]-pdu[\[Alpha]]@inds[h,{\[Mu],\[Nu]},"ul"])*)
(*%//.rule@{linearHead,leibnizHead,derivHead}*)*)


(* ::Subsection::Closed:: *)
(*Calculations*)


(* ::Subsubsection:: *)
(*Calculate form of Levi-Civita connection*)


(* ::Input:: *)
(*(*\[Del]t=OverTilde["\[Del]"]*)*)


(* ::Input:: *)
(*(*(* Rule for expressing the full covar deriv in terms of a differernt connection *)*)
(*rule[cdldt]=p:cdl[a_]@(T:inds[A_,indList_,pos_])\[RuleDelayed]cdl[a,\[Del]t]@T+Module[{indsPos=Transpose@{indList,fullIndsPos[pos,Length@indList],Range@Length@indList},\[Mu]0=unique@"\[Mu]"},Plus@@Function[{ind,ul,i},If[ul\[Equal]"u",1,-1]*inds[CC,If[ul\[Equal]"u",{ind,a,\[Mu]0},{\[Mu]0,a,ind}],"ull"]inds[A,ReplacePart[i\[Rule]\[Mu]0]@indList,pos] ]@@@indsPos]*)*)


(* ::Input:: *)
(*(* Calculate Riemann tensor for two different connections *)*)


(* ::Input:: *)
(*(*cdl[a]@cdl[b]@indl[c]@w*)
(*%/.rule[cdldt]*)
(*%//replaceDummyInds*)
(*%/.covarDeriv[inds[A_,indList_,posList_],cind_,cpos_,\[Del]t]\[RuleDelayed]inds[Function[covarDeriv[inds[A,{##2},posList],#1,cpos,\[Del]t]],{cind}~Join~indList,{cpos}~StringJoin~posList]*)
(*%//.rule@{leibnizHead,linearHead,derivHead}*)
(*%/.rule[cdldt]*)
(*%//replaceDummyInds*)
(*%//Expand*)
(*%//replaceDummyInds*)
(*%//countInds*)
(*%%;*)
(*%/.inds[f_Function,indList_,pos_]\[RuleDelayed]f@@indList*)
(*temp=%*)
(**)*)


(* ::Input:: *)
(*(*temp-(temp/.{a\[Rule]b,b\[Rule]a})//Expand//Map[replaceDummyInds]*)
(*%/.inds[CC,{a_,b__},pos_]\[RuleDelayed]inds[CC,{a}~Join~Sort@{b},pos]*)
(*Replace[expr_/;!FreeQ[expr,indl[\[Nu]]@w]\[RuleDelayed](expr/.{\[Nu]\[Rule]\[Mu],\[Mu]\[Rule]\[Nu]})]/@%*)
(*%/.cdl[a,\[Del]t]@cdl[b,\[Del]t]@indl[c]@w\[Rule]Module[{d=\[Mu]},inds[Rt,{a,b,c,d},"lllu"]indl[d]@w]+cdl[b,\[Del]t]@cdl[a,\[Del]t]@indl[c]@w*)
(*%//replaceDummyInds*)
(*Simplify@%*)*)


(* ::Subsubsection:: *)
(*Calculate a Riemann Tensor*)


(* ::Input:: *)
(*(**)
(*ruleCalc["\[CapitalGamma]lll",metric_]:=With[{gg=metric},inds["\[CapitalGamma]",{\[Alpha]_,\[Mu]_,\[Nu]_},"l"]\[RuleDelayed]1/2(pdl[\[Mu]]@gg[\[Alpha],\[Nu]]+pdl[\[Nu]]@gg[\[Mu],\[Alpha]]-pdl[\[Alpha]]@gg[\[Mu],\[Nu]])]*)
(*ruleCalc["\[CapitalGamma]ull",metric_]:=inds["\[CapitalGamma]",{\[Alpha]_,\[Mu]_,\[Nu]_},"ul"]\[RuleDelayed]With[{gg=metric},Module[{\[Beta]},inverseMetric[gg][\[Alpha],\[Beta]]inds["\[CapitalGamma]",{\[Beta],\[Mu],\[Nu]},"l"]]]*)
(*ruleCalc["Rhalfulll",metric_]:=inds["Rhalf",{\[Alpha]_,\[Beta]_,\[Mu]_,\[Nu]_},"ulll"]\[RuleDelayed]Module[{\[Gamma]},pdl[\[Mu]]@inds["\[CapitalGamma]",{\[Alpha],\[Nu],\[Beta]},"ul"]+inds["\[CapitalGamma]",{\[Alpha],\[Mu],\[Gamma]},"ul"]inds["\[CapitalGamma]",{\[Gamma],\[Nu],\[Beta]},"ul"] ]*)
(*ruleCalc["Rulll",metric_]:=inds["R",{\[Alpha]_,\[Beta]_,\[Mu]_,\[Nu]_},"ulll"]\[RuleDelayed]inds["Rhalf",{\[Alpha],\[Beta],\[Mu],\[Nu]},"ulll"]-inds["Rhalf",{\[Alpha],\[Beta],\[Nu],\[Mu]},"ulll"]*)*)


(* ::Input:: *)
(*(*inds["R",{\[Alpha],\[Beta],\[Mu],\[Nu]},"ulll"]*)
(*%/.ruleCalc["Rulll",];*)
(*%/.ruleCalc["Rhalfulll",];*)
(*%/.ruleCalc["\[CapitalGamma]ull",gg];*)
(*%/.ruleCalc["\[CapitalGamma]lll",gg];*)
(*%/.inverseMetric@gg\[Rule](inds[gginv,{#1,#2},"u"]&);*)
(*%/.gg\[Rule](inds[gg,{#1,#2},"l"]&)*)
(*%//.rule@{derivHead,leibnizHead,linearHead}*)
(*%//Expand*)
(*temp=%*)*)


(* ::Input:: *)
(*(*ex*)*)


(* ::Input:: *)
(*(*temp;*)
(*extractNonDummyIndsSpec/@(List@@%)*)
(*Sort/@%;*)
(*Union@%;*)
(*Length@%*)*)


(* ::Input:: *)
(*(*f//g@h*)*)


(* ::Input:: *)
(*(*temp//Map@replaceDummyInds*)
(*inds["R",{\[Mu],\[Nu]},"l"]*)
(*%%;*)
(*%/.\[Mu]\[Rule]\[Alpha]/.\[Beta]\[Rule]\[Mu]*)
(*ricci\[Mu]\[Nu]=temp2=%;*)*)


(* ::Input:: *)
(*(*ricci\[Mu]\[Nu]/.Table[{\[Mu]\[Rule]\[Mu]i,\[Nu]\[Rule]\[Nu]i},{\[Mu]i,{0,1}},{\[Nu]i,{0,1}}];*)
(*%/.indexDeriv[inds[gg|gginv,__],x,0,_]\[Rule]0*)*)


(* ::Input:: *)
(*(*rule/@{derivHead,leibnizHead,linearHead}//Column*)*)


(* ::Input:: *)
(*(*?rule*)*)


(* ::Input:: *)
(*(*pdl*)*)


(* ::Section:: *)
(*End package*)


(* ::Input:: *)
(*(*EndPackage[]*)*)


(* ::Input:: *)
(**)
