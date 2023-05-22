*weight for age
keep if (_zwei>=-5 & _zwei<=5)
hist _zwei
histogram _zwei, bin(100) normal
histogram _zwei, bin(500) normal
histogram _zwei, bin(100) normal
histogram _zwei, bin(100) normal xline(-2)
histogram _zwei, bin(100) normal

*length for height
keep if ( _zlen >=-5 & _zlen <=5)
histogram _zlen , bin(100) normal

* weight for length
keep if ( _zwfl >=-5 & _zwfl <=5)
histogram _zwfl , bin(100) normal
histogram _zwfl , bin(100) normal
