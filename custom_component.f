!========================================================
! State  P	Z	N
!   1	 A,B		C
!   2	  C	       A,B
!   3	 A,C		B
!   4	  B	       A,C
!   5	 B,C		A
!   6	  A	       B,C
!   7	  B	A	C
!   8	  C	A	B
!   9	  A	B	C
!   10	  C	B	A
!   11	  A	C	B
!   12	  B	C	A
! -----------------------------
! 9 -> 1 -> 7 -> 4 -> 12 -> 5 -> 10 -> 2 -> 8 -> 3 -> 11 -> 6
!========================================================

#LOCAL INTEGER SaT_local 4
#LOCAL INTEGER SbT_local 4
#LOCAL INTEGER ScT_local 4

#LOCAL REAL Eeab_calc
#LOCAL REAL Eebc_calc
#LOCAL REAL Eeca_calc

#LOCAL REAL deltaEphs_calc
#LOCAL REAL Vmid_ref_calc
#LOCAL REAL K_MID_P

#LOCAL REAL Icir_calc
#LOCAL REAL deltaEphscir_calc
#LOCAL REAL Icir_ref_calc
#LOCAL REAL Icir_err_calc
#LOCAL REAL VcirPN_calc
#LOCAL REAL K_CIR_E
#LOCAL REAL K_CIR_P

#LOCAL REAL deltaEarm_calc
#LOCAL REAL deltaV_ref_calc

#LOCAL REAL K_ARM_Z_P


!========================================================
! Internal Phase Balancing Calculation
! Replacement of external Middle Point / Phase Balancing Controller
!========================================================
! Required inputs: Ea, Eb, Ec
! P-only first test: Vmid_ref_calc = 0.1 * deltaEphs_calc
! Original external controller had P + I:
! Vmid_ref = 0.1*deltaEphs + 50*integral(deltaEphs)
!========================================================

K_MID_P = 0.1

Eeab_calc = $Ea - $Eb
Eebc_calc = $Eb - $Ec
Eeca_calc = $Ec - $Ea

deltaEphs_calc = 0.0

IF ($State == 7) THEN
	deltaEphs_calc = Eebc_calc
ENDIF

IF ($State == 8) THEN
	deltaEphs_calc = - Eebc_calc
ENDIF

IF ($State == 10) THEN
	deltaEphs_calc = Eeca_calc
ENDIF

IF ($State == 9) THEN
	deltaEphs_calc = - Eeca_calc
ENDIF

IF ($State == 11) THEN
	deltaEphs_calc = Eeab_calc
ENDIF

IF ($State == 12) THEN
	deltaEphs_calc = - Eeab_calc
ENDIF

IF ($State == 1) THEN
	deltaEphs_calc = (Eebc_calc - Eeca_calc) / 2.0
ENDIF

IF ($State == 2) THEN
	deltaEphs_calc = (- Eebc_calc + Eeca_calc) / 2.0
ENDIF

IF ($State == 3) THEN
	deltaEphs_calc = (Eeab_calc - Eebc_calc) / 2.0
ENDIF

IF ($State == 4) THEN
	deltaEphs_calc = (- Eeab_calc + Eebc_calc) / 2.0
ENDIF

IF ($State == 5) THEN
	deltaEphs_calc = (Eeca_calc - Eeab_calc) / 2.0
ENDIF

IF ($State == 6) THEN
	deltaEphs_calc = (- Eeca_calc + Eeab_calc) / 2.0
ENDIF

Vmid_ref_calc = K_MID_P * deltaEphs_calc


!========================================================
! Internal P/N Arm Balancing Calculation
!========================================================
! Original logic:
! State 1,2 : Icir = -Ica + Icb, deltaEphscir = Eeab
! State 3,4 : Icir = -Icc + Ica, deltaEphscir = Eeca
! State 5,6 : Icir = -Icb + Icc, deltaEphscir = Eebc

! Original gain:
! Icir_ref = 0.00001 * deltaEphscir
! VcirPN_calc = 0.0001 * (Icir_ref - Icir)
!========================================================

K_CIR_E = 0.00001
K_CIR_P = 0.0001

Icir_calc = 0.0
deltaEphscir_calc = 0.0
Icir_ref_calc = 0.0
Icir_err_calc = 0.0
VcirPN_calc = 0.0

IF (($State == 1) .OR. ($State == 2)) THEN
	Icir_calc = - $Ica + $Icb
	deltaEphscir_calc = Eeab_calc
ENDIF

IF (($State == 3) .OR. ($State == 4)) THEN
	Icir_calc = - $Icc + $Ica
	deltaEphscir_calc = Eeca_calc
ENDIF

IF (($State == 5) .OR. ($State == 6)) THEN
	Icir_calc = - $Icb + $Icc
	deltaEphscir_calc = Eebc_calc
ENDIF

Icir_ref_calc = K_CIR_E * deltaEphscir_calc
Icir_err_calc = Icir_ref_calc - Icir_calc
VcirPN_calc = K_CIR_P * Icir_err_calc


!========================================================
! Internal Z-State Arm Balancing Calculation
! Replacement of external Arm Energy Controller / deltaV_ref
!========================================================
! Original Arm Energy Controller:
! State 7,8   : deltaEarm = Eeapn
! State 9,10  : deltaEarm = Eebpn
! State 11,12 : deltaEarm = Eecpn
!
! This controller acts only in Z state.
! First replacement test:
!   deltaV_ref_calc = K_ARM_Z_P * deltaEarm_calc
!
! From the existing block diagram, the external controller appears to use:
!   P gain = 0.001
!   I gain = 0.001
! Here, start with P-only to avoid integrator implementation error.
!========================================================

K_ARM_Z_P = 0.001

deltaEarm_calc = 0.0
deltaV_ref_calc = 0.0


!------------------ Phase A is Z -------------------
IF (($State == 7) .OR. ($State == 8)) THEN
	deltaEarm_calc = $Eeapn
ENDIF


!------------------ Phase B is Z -------------------
IF (($State == 9) .OR. ($State == 10)) THEN
	deltaEarm_calc = $Eebpn
ENDIF


!------------------ Phase C is Z -------------------
IF (($State == 11) .OR. ($State == 12)) THEN
	deltaEarm_calc = $Eecpn
ENDIF


!------------------ Convert Z-state arm energy error to compensation voltage -------------------

deltaV_ref_calc = K_ARM_Z_P * deltaEarm_calc





!========================================================
! Converter State Machine
! External Vmid_ref is replaced by Vmid_ref_calc.
! External VcirPN is replaced by VcirPN_calc.
! External deltaV_ref is replaced by deltaV_ref_calc.
!========================================================

IF ($State == 1) THEN
	$Vap_ref = 0.5*$Vdc_ref - $Va_ref - VcirPN_calc
	$Van_ref = $Va_ref - Vmid_ref_calc - VcirPN_calc
	$Vbp_ref = 0.5*$Vdc_ref - $Vb_ref + VcirPN_calc
	$Vbn_ref = $Vb_ref - Vmid_ref_calc + VcirPN_calc
	$Vcp_ref = - $Vc_ref + Vmid_ref_calc
	$Vcn_ref = $Vc_ref + 0.5*$Vdc_ref
ENDIF

IF ($State == 2) THEN
	$Vap_ref = - $Va_ref + Vmid_ref_calc - VcirPN_calc
	$Van_ref = $Va_ref + 0.5*$Vdc_ref - VcirPN_calc
	$Vbp_ref = - $Vb_ref + Vmid_ref_calc + VcirPN_calc
	$Vbn_ref = $Vb_ref + 0.5*$Vdc_ref + VcirPN_calc
	$Vcp_ref = 0.5*$Vdc_ref - $Vc_ref
	$Vcn_ref = $Vc_ref - Vmid_ref_calc
ENDIF

IF ($State == 3) THEN
	$Vap_ref = 0.5*$Vdc_ref - $Va_ref + VcirPN_calc
	$Van_ref = $Va_ref - Vmid_ref_calc + VcirPN_calc
	$Vbp_ref = - $Vb_ref + Vmid_ref_calc
	$Vbn_ref = $Vb_ref + 0.5*$Vdc_ref
	$Vcp_ref = 0.5*$Vdc_ref - $Vc_ref - VcirPN_calc
	$Vcn_ref = $Vc_ref - Vmid_ref_calc - VcirPN_calc
ENDIF

IF ($State == 4) THEN
	$Vap_ref = - $Va_ref + Vmid_ref_calc + VcirPN_calc
	$Van_ref = $Va_ref + 0.5*$Vdc_ref + VcirPN_calc
	$Vbp_ref = 0.5*$Vdc_ref - $Vb_ref
	$Vbn_ref = $Vb_ref - Vmid_ref_calc
	$Vcp_ref = - $Vc_ref + Vmid_ref_calc - VcirPN_calc
	$Vcn_ref = $Vc_ref + 0.5*$Vdc_ref - VcirPN_calc
ENDIF

IF ($State == 5) THEN
	$Vap_ref = - $Va_ref + Vmid_ref_calc
	$Van_ref = $Va_ref + 0.5*$Vdc_ref
	$Vbp_ref = 0.5*$Vdc_ref - $Vb_ref - VcirPN_calc
	$Vbn_ref = $Vb_ref - Vmid_ref_calc - VcirPN_calc
	$Vcp_ref = 0.5*$Vdc_ref - $Vc_ref + VcirPN_calc
	$Vcn_ref = $Vc_ref - Vmid_ref_calc + VcirPN_calc
ENDIF

IF ($State == 6) THEN
	$Vap_ref = 0.5*$Vdc_ref - $Va_ref
	$Van_ref = $Va_ref - Vmid_ref_calc
	$Vbp_ref = - $Vb_ref + Vmid_ref_calc - VcirPN_calc
	$Vbn_ref = $Vb_ref + 0.5*$Vdc_ref - VcirPN_calc
	$Vcp_ref = - $Vc_ref + Vmid_ref_calc + VcirPN_calc
	$Vcn_ref = $Vc_ref + 0.5*$Vdc_ref + VcirPN_calc
ENDIF

IF ($State == 7) THEN
	$Vap_ref = - $Va_ref + Vmid_ref_calc - deltaV_ref_calc
	$Van_ref = $Va_ref - Vmid_ref_calc + deltaV_ref_calc
	$Vbp_ref = 0.5*$Vdc_ref - $Vb_ref
	$Vbn_ref = $Vb_ref - Vmid_ref_calc
	$Vcp_ref = - $Vc_ref + Vmid_ref_calc
	$Vcn_ref = $Vc_ref + 0.5*$Vdc_ref
ENDIF

IF ($State == 8) THEN
	$Vap_ref = - $Va_ref + Vmid_ref_calc - deltaV_ref_calc
	$Van_ref = $Va_ref - Vmid_ref_calc + deltaV_ref_calc
	$Vbp_ref = - $Vb_ref + Vmid_ref_calc
	$Vbn_ref = $Vb_ref + 0.5*$Vdc_ref
	$Vcp_ref = 0.5*$Vdc_ref - $Vc_ref
	$Vcn_ref = $Vc_ref - Vmid_ref_calc
ENDIF

IF ($State == 9) THEN
	$Vap_ref = 0.5*$Vdc_ref - $Va_ref
	$Van_ref = $Va_ref - Vmid_ref_calc
	$Vbp_ref = - $Vb_ref + Vmid_ref_calc - deltaV_ref_calc
	$Vbn_ref = $Vb_ref - Vmid_ref_calc + deltaV_ref_calc
	$Vcp_ref = - $Vc_ref + Vmid_ref_calc
	$Vcn_ref = $Vc_ref + 0.5*$Vdc_ref
ENDIF

IF ($State == 10) THEN
	$Vap_ref = - $Va_ref + Vmid_ref_calc
	$Van_ref = $Va_ref + 0.5*$Vdc_ref
	$Vbp_ref = - $Vb_ref + Vmid_ref_calc - deltaV_ref_calc
	$Vbn_ref = $Vb_ref - Vmid_ref_calc + deltaV_ref_calc
	$Vcp_ref = 0.5*$Vdc_ref - $Vc_ref
	$Vcn_ref = $Vc_ref - Vmid_ref_calc
ENDIF

IF ($State == 11) THEN
	$Vap_ref = 0.5*$Vdc_ref - $Va_ref
	$Van_ref = $Va_ref - Vmid_ref_calc
	$Vbp_ref = - $Vb_ref + Vmid_ref_calc
	$Vbn_ref = $Vb_ref + 0.5*$Vdc_ref
	$Vcp_ref = - $Vc_ref + Vmid_ref_calc - deltaV_ref_calc
	$Vcn_ref = $Vc_ref - Vmid_ref_calc + deltaV_ref_calc
ENDIF

IF ($State == 12) THEN
	$Vap_ref = - $Va_ref + Vmid_ref_calc
	$Van_ref = $Va_ref + 0.5*$Vdc_ref
	$Vbp_ref = 0.5*$Vdc_ref - $Vb_ref
	$Vbn_ref = $Vb_ref - Vmid_ref_calc
	$Vcp_ref = - $Vc_ref + Vmid_ref_calc - deltaV_ref_calc
	$Vcn_ref = $Vc_ref - Vmid_ref_calc + deltaV_ref_calc
ENDIF


!------------------ Switching signals for Phase A -------------------
IF (($State == 1) .OR. ($State == 3) .OR. ($State == 6) .OR. ($State == 9) .OR. ($State == 11)) THEN
	SaT_local(1) = 1
	SaT_local(2) = 0
	SaT_local(3) = 1
	SaT_local(4) = 0
ELSE
	IF (($State == 7) .OR. ($State == 8)) THEN
		SaT_local(1) = 0
		SaT_local(2) = 1
		SaT_local(3) = 1
		SaT_local(4) = 0
	ELSE
		SaT_local(1) = 0
		SaT_local(2) = 1
		SaT_local(3) = 0
		SaT_local(4) = 1
	ENDIF
ENDIF

!------------------ Switching signals for Phase B -------------------
IF (($State == 1) .OR. ($State == 4) .OR. ($State == 5) .OR. ($State == 7) .OR. ($State == 12)) THEN
	SbT_local(1) = 1
	SbT_local(2) = 0
	SbT_local(3) = 1
	SbT_local(4) = 0
ELSE
	IF (($State == 9) .OR. ($State == 10)) THEN
		SbT_local(1) = 0
		SbT_local(2) = 1
		SbT_local(3) = 1
		SbT_local(4) = 0
	ELSE
		SbT_local(1) = 0
		SbT_local(2) = 1
		SbT_local(3) = 0
		SbT_local(4) = 1
	ENDIF
ENDIF

!------------------ Switching signals for Phase C -------------------
IF (($State == 2) .OR. ($State == 3) .OR. ($State == 5) .OR. ($State == 8) .OR. ($State == 10)) THEN
	ScT_local(1) = 1
	ScT_local(2) = 0
	ScT_local(3) = 1
	ScT_local(4) = 0
ELSE
	IF (($State == 11) .OR. ($State == 12)) THEN
		ScT_local(1) = 0
		ScT_local(2) = 1
		ScT_local(3) = 1
		ScT_local(4) = 0
	ELSE
		ScT_local(1) = 0
		ScT_local(2) = 1
		ScT_local(3) = 0
		ScT_local(4) = 1
	ENDIF
ENDIF

$SaT = SaT_local
$SbT = SbT_local
$ScT = ScT_local
