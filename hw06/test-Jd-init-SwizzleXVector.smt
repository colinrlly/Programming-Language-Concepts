(val sxv01 unit)
(begin (set sxv01 (withXV1:withXV2: SwizzleXVector xv01 xv01)) (add: xvs sxv01) (add: ns 'sxv01) unit)
(val sxv02 unit)
(begin (set sxv02 (withXV1:withXV2: SwizzleXVector xv01 xv04)) (add: xvs sxv02) (add: ns 'sxv02) unit)
(val sxv03 unit)
(begin (set sxv03 (withXV1:withXV2: SwizzleXVector xv04 xv01)) (add: xvs sxv03) (add: ns 'sxv03) unit)
(val sxv04 unit)
(begin (set sxv04 (withXV1:withXV2: SwizzleXVector xv03 xv03)) (add: xvs sxv04) (add: ns 'sxv04) unit)
(val sxv05 unit)
(begin (set sxv05 (withXV1:withXV2: SwizzleXVector xv03 xv07)) (add: xvs sxv05) (add: ns 'sxv05) unit)
(val sxv06 unit)
(begin (set sxv06 (withXV1:withXV2: SwizzleXVector xv07 xv03)) (add: xvs sxv06) (add: ns 'sxv06) unit)
(val sxv07 unit)
(begin (set sxv07 (withXV1:withXV2: SwizzleXVector xv07 xv07)) (add: xvs sxv07) (add: ns 'sxv07) unit)
