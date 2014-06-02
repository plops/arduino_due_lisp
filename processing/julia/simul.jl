using GSL

function v(ncore, nclad; r_core=31.25e-3, wavelength=633e-6)
    # r_core and wavelength in mm
    2*pi/wavelength * r_core * sqrt(ncore^2-nclad^2)
end

function v(Delta=.02, r_core=31.25e-3, wavelength=633e-6, n_core=1.498)
    2*pi/wavelength * r_core * sqrt(2*Delta) * n_core
end


function v(NA=.275, r_core=31.25e-3, wavelength=633e-6)
    2*pi/wavelength * r_core * sqrt(2*Delta) * n_core
end

# calculate 
.275/sqrt(2*.02) # => nco=1.375 i would expect ~1.5, this doesn't make sense


GSL.sf_bessel_zero_Jnu(2,1:10)
v()
