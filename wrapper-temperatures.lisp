(in-package :arv)
(defmethod %basler-temperatures ((cam camera))
  (loop for (i name) in '((0 sensor)
			  (1 core)
			  (2 framegrabber)
			  (3 case))
	collect 
	(progn
	  (gc-enumeration-set-int-value cam "TemperatureSelector" i)
	  (list name
		(gc-float-get-value cam "TemperatureAbs")))))

(defmethod %photonfocus-temperatures ((cam camera))
  (loop for (i name) in '((0 sensor)
			  (1 sensor-board)
			  (2 adc-board))
     collect 
       (progn
	 (gc-integer-set-value cam "DeviceTemperatureSelectorVal" i)
	 (gc-command-execute cam "DeviceTemperature_Update")
	 (list
	  name
	  (gc-float-get-value cam "DeviceTemperature")))))

(defmethod temperatures ((cam camera))
  (cond
    ((string= "Photonfocus AG" (arv-vendor-name cam))
     (%photonfocus-temperatures cam))
    ((string= "Basler" (arv-vendor-name cam))
     (%basler-temperatures cam))))
