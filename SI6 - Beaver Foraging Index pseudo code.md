## SI6 - Beaver Foraging Index (BFI) pseudo code describing the inference system used to derive the final GB BFI Raster file.md
<br/>

**1)	TCD conversion – convert Tree cover Density (TCD) from continuous description of cover (TCDi) to a value between Null-4 (TCDo).**
*	If TCDi  = 0% then TCDo = Null
*	If TCDi  > 0%  and < 3% then TCDo = 1
*	If TCDi  >= 3%  and < 10% then TCDo = 2
*	If TCDi  >= 10%  and < 50% then TCDo = 3
*	If TCDi  >= 50%  then TCDo = 4 <br/>
**2)	To produce the next intermediate data (referred to here as VA), TCDo is combined with the other classified datasets (OS Vector, CEH Land Cover Map (LCM), CEH Woody Linar Features Framework (WLF)). Layers are simply merged based on the reliability of the data. Where no data is present, the next most reliable data is used to fill this step. As the LCM is continuous and the coarsest in detail, it is final layer to be selected if the other datasets are of NULL value in a given location. This inference step takes place as follows:**
*	If OS is not NULL then VA = OS
*	If OS is NULL then VA = LWF
*	If WLF is NULL then VA = TCD
*	If TCD is NULL then VA = LCM <br/>
**3)	However, depending on the landuse types, other data may be more reliable at predicting the presence of woody material or buildings. As false positives are rare in all datasets it was decided to, as a secondary process, prioritise those data that are of higher value. The following sequence of commands was used to achieve this where VB:VD are intermediate datasets:**
*	If VA < WLF then VB = LWF else VB = VA
*	If VB < TCD and ConLCM = Null then VC = TCD else VC = VB
*	If VC < LCM then VD = LCM else VD = VC
*	If OS < 1 then BVI = OS else BVI = VD
