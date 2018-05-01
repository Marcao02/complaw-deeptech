Focusing on KISS Equity version

From KISS Equity document

KISS Fully-diluted capitalization - ONLY includes shares of Common Stock
“Fully-Diluted Capitalization” shall mean the number of shares of outstanding *Common Stock* of the Company on a fully-diluted basis, including
	(i) conversion or exercise of all securities convertible into or exercisable for Common Stock,
	(ii) exercise of all outstanding options and warrants to purchase Common Stock and,
	(iii) in the case of Section 1(b)(i) and 1(b)(iii) only, the shares reserved or authorized for issuance under the Company’s existing stock option plan or any stock option plan created or increased in connection with such transaction;
but excluding, for this purpose, the conversion contemplated by the applicable provision of Section 2.


kissFDC is just a parameter.

conv₁ =  inv₁ /
         (((cap₁cap₂ - inv₁inv₂)/(cap₂+inv₂))/E)


500 / (( (2000*3000 - 200*500) / (2000+200) ) / 100)
200 / (( (2000*3000 - 200*500) / (3000+500) ) / 100)

200 / (2000 / 100)











KISSCap := ...in *most* cases excludes preferred stock arising from SAFEs and other KISSes.

SAFELiqCap := ...includes preferred stock by default


Ss is the number of shares allocated to the SAFE investor
Ks is the number of shares allocated to the KISS investor
¬K¬Ss is the number of projected non-KISS non-SAFE shares
rS is (investment of SAFE investor / price that SAFE investor gets)
rS is (investment of KISS investor / price that KISS investor gets)

