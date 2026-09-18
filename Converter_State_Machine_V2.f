VERSION:
3.001

! NOTE: The C definition includes "Converter_State_Machine_V2.h" here.
! There is no direct Fortran equivalent for a C header include in this
! section. If that header defines shared constants/macros also needed
! here, port them manually (e.g. as PARAMETER declarations below) --
! this could not be verified without the header's contents.

STATIC:

! ======================================================
!  Soft Start
! ======================================================

REAL*8  Vcap_avg
INTEGER SoftStartDone
INTEGER System_State

REAL*8  Vmax
REAL*8  Vlimit

! ======================================================
!  Internal Phase Balancing
! ======================================================

REAL*8  Eeap, Eean
REAL*8  Eebp, Eebn
REAL*8  Eecp, Eecn
REAL*8  Ee

REAL*8  dir_ap, dir_an
REAL*8  dir_bp, dir_bn
REAL*8  dir_cp, dir_cn

REAL*8  VP1, VP2
REAL*8  VN1, VN2

REAL*8  EP1, EP2
REAL*8  EN1, EN2

REAL*8  dirP1, dirP2
REAL*8  dirN1, dirN2

REAL*8  wP1, wP2
REAL*8  wN1, wN2

REAL*8  sum_w
INTEGER Type

REAL*8  VdesP1, VdesP2
REAL*8  VdesN1, VdesN2

REAL*8  Vupper
REAL*8  Vlower

REAL*8  Vmid_ref

! ======================================================
!  Modulation
! ======================================================

REAL*8  Vap_ref, Van_ref
REAL*8  Vbp_ref, Vbn_ref
REAL*8  Vcp_ref, Vcn_ref

! --------------------------------------------------
!  Inserted SM numbers
! --------------------------------------------------
INTEGER numap
INTEGER numan
INTEGER numbp
INTEGER numbn
INTEGER numcp
INTEGER numcn

! --------------------------------------------------
!  Internal variables
! --------------------------------------------------
INTEGER base
INTEGER pwm_flag
INTEGER sic_en

REAL*8  VSI_AVG
REAL*8  VDIFF

REAL*8  Vlevel

REAL*8  max_cap_voltage

INTEGER Sa11_cal
INTEGER Sa12_cal
INTEGER Sa21_cal
INTEGER Sa22_cal
INTEGER Sa31_cal
INTEGER Sa32_cal
INTEGER Sa41_cal
INTEGER Sa42_cal
INTEGER Sa51_cal
INTEGER Sa52_cal
INTEGER Sa61_cal
INTEGER Sa62_cal
INTEGER Sb11_cal
INTEGER Sb12_cal
INTEGER Sb21_cal
INTEGER Sb22_cal
INTEGER Sb31_cal
INTEGER Sb32_cal
INTEGER Sb41_cal
INTEGER Sb42_cal
INTEGER Sb51_cal
INTEGER Sb52_cal
INTEGER Sb61_cal
INTEGER Sb62_cal
INTEGER Sc11_cal
INTEGER Sc12_cal
INTEGER Sc21_cal
INTEGER Sc22_cal
INTEGER Sc31_cal
INTEGER Sc32_cal
INTEGER Sc41_cal
INTEGER Sc42_cal
INTEGER Sc51_cal
INTEGER Sc52_cal
INTEGER Sc61_cal
INTEGER Sc62_cal

RAM_FUNCTIONS:


RAM:

SoftStartDone = 0

! --------------------------------------------------------
! Common threshold for P/Z/N decision
! --------------------------------------------------------
Vlevel = 125.0
Ee = 375.0
max_cap_voltage = 800.0

CODE:

! ======================================================
!  Soft Start State Machine
! ======================================================

! Average capacitor voltage

Vcap_avg = (Va1 + Va2 + Va3 + Va4 + Va5 + Va6 + Vb1 + Vb2 + Vb3 + &
           Vb4 + Vb5 + Vb6 + Vc1 + Vc2 + Vc3 + Vc4 + Vc5 + Vc6) / 18.0

! System state

IF (SoftStartDone == 0) THEN
    IF (Vcap_avg < 60) THEN
        System_State = 0
    ELSE IF (Vcap_avg < 120) THEN
        System_State = 1
    ELSE
        System_State = 2
        SoftStartDone = 1
    END IF
ELSE
    System_State = 2
END IF

! --------------------------------------------------
!  Reset switching outputs
! --------------------------------------------------
!---------------- Phase A ----------------
Sa11_cal = 0.0
Sa12_cal = 0.0
Sa21_cal = 0.0
Sa22_cal = 0.0
Sa31_cal = 0.0
Sa32_cal = 0.0
Sa41_cal = 0.0
Sa42_cal = 0.0
Sa51_cal = 0.0
Sa52_cal = 0.0
Sa61_cal = 0.0
Sa62_cal = 0.0

!---------------- Phase B ----------------
Sb11_cal = 0.0
Sb12_cal = 0.0
Sb21_cal = 0.0
Sb22_cal = 0.0
Sb31_cal = 0.0
Sb32_cal = 0.0
Sb41_cal = 0.0
Sb42_cal = 0.0
Sb51_cal = 0.0
Sb52_cal = 0.0
Sb61_cal = 0.0
Sb62_cal = 0.0

!---------------- Phase C ----------------
Sc11_cal = 0.0
Sc12_cal = 0.0
Sc21_cal = 0.0
Sc22_cal = 0.0
Sc31_cal = 0.0
Sc32_cal = 0.0
Sc41_cal = 0.0
Sc42_cal = 0.0
Sc51_cal = 0.0
Sc52_cal = 0.0
Sc61_cal = 0.0
Sc62_cal = 0.0

! ======================================================
!  Soft Start : State 0
! ======================================================

IF ((System_State == 0) .AND. (SoftStartDone == 0)) THEN

    ! Disable all switches

    SaT1 = 1
    SaT2 = 0
    SaT3 = 0
    SaT4 = 1

    SbT1 = 1
    SbT2 = 0
    SbT3 = 0
    SbT4 = 1

    ScT1 = 1
    ScT2 = 0
    ScT3 = 0
    ScT4 = 1

    ! Voltage references

    Vap_ref = 0.0
    Van_ref = 0.0

    Vbp_ref = 0.0
    Vbn_ref = 0.0

    Vcp_ref = 0.0
    Vcn_ref = 0.0

    ! Keep breakers open

    Brk_AC = 0
    Brk_DC = 0

    !---------------- Phase A ----------------
    IF (Icap > 0.0) THEN
        Sa11_cal = 1.0
        Sa21_cal = 1.0
        Sa31_cal = 1.0
    END IF

    IF (Ican > 0.0) THEN
        Sa41_cal = 1.0
        Sa51_cal = 1.0
        Sa61_cal = 1.0
    END IF

    !---------------- Phase B ----------------
    IF (Icbp > 0.0) THEN
        Sb11_cal = 1.0
        Sb21_cal = 1.0
        Sb31_cal = 1.0
    END IF

    IF (Icbn > 0.0) THEN
        Sb41_cal = 1.0
        Sb51_cal = 1.0
        Sb61_cal = 1.0
    END IF

    !---------------- Phase C ----------------
    IF (Iccp > 0.0) THEN
        Sc11_cal = 1.0
        Sc21_cal = 1.0
        Sc31_cal = 1.0
    END IF

    IF (Iccn > 0.0) THEN
        Sc41_cal = 1.0
        Sc51_cal = 1.0
        Sc61_cal = 1.0
    END IF

! ======================================================
!  Soft Start : State 1
! ======================================================

ELSE IF ((System_State == 1) .AND. (SoftStartDone == 0)) THEN

    ! Fixed switching state

    SaT1 = 1
    SaT2 = 0
    SaT3 = 0
    SaT4 = 1

    SbT1 = 1
    SbT2 = 0
    SbT3 = 0
    SbT4 = 1

    ScT1 = 1
    ScT2 = 0
    ScT3 = 0
    ScT4 = 1

    ! Voltage references

    Vap_ref = 0.0
    Van_ref = 0.0

    Vbp_ref = 0.0
    Vbn_ref = 0.0

    Vcp_ref = 0.0
    Vcn_ref = 0.0

    ! Keep breakers open

    Brk_AC = 0
    Brk_DC = 0

    !==========================
    ! A PHASE - UPPER ARM
    !==========================

    Vmax = Va1
    IF (Va2 > Vmax) Vmax = Va2
    IF (Va3 > Vmax) Vmax = Va3

    Vlimit = 0.95 * Vmax

    ! Only Va1 is low
    IF ((Va1 < Vlimit) .AND. (Va2 >= Vlimit) .AND. (Va3 >= Vlimit) .AND. (Icap > 0.0)) THEN
        Sa11_cal = 1.0
        Sa22_cal = 1.0
        Sa32_cal = 1.0

    ! Only Va2 is low
    ELSE IF ((Va2 < Vlimit) .AND. (Va1 >= Vlimit) .AND. (Va3 >= Vlimit) .AND. (Icap > 0.0)) THEN
        Sa12_cal = 1.0
        Sa21_cal = 1.0
        Sa32_cal = 1.0

    ! Only Va3 is low
    ELSE IF ((Va3 < Vlimit) .AND. (Va1 >= Vlimit) .AND. (Va2 >= Vlimit) .AND. (Icap > 0.0)) THEN
        Sa12_cal = 1.0
        Sa22_cal = 1.0
        Sa31_cal = 1.0

    ! Va1 & Va2 are low
    ELSE IF ((Va1 < Vlimit) .AND. (Va2 < Vlimit) .AND. (Va3 >= Vlimit) .AND. (Icap > 0.0)) THEN
        Sa11_cal = 1.0
        Sa21_cal = 1.0
        Sa32_cal = 1.0

    ! Va1 & Va3 are low
    ELSE IF ((Va1 < Vlimit) .AND. (Va3 < Vlimit) .AND. (Va2 >= Vlimit) .AND. (Icap > 0.0)) THEN
        Sa11_cal = 1.0
        Sa22_cal = 1.0
        Sa31_cal = 1.0

    ! Va2 & Va3 are low
    ELSE IF ((Va2 < Vlimit) .AND. (Va3 < Vlimit) .AND. (Va1 >= Vlimit) .AND. (Icap > 0.0)) THEN
        Sa12_cal = 1.0
        Sa21_cal = 1.0
        Sa31_cal = 1.0

    ! All three are low or balanced
    ELSE IF (Icap > 0.0) THEN
        Sa11_cal = 1.0
        Sa21_cal = 1.0
        Sa31_cal = 1.0

    END IF

    !==========================
    ! A PHASE - LOWER ARM
    !==========================

    Vmax = Va4
    IF (Va5 > Vmax) Vmax = Va5
    IF (Va6 > Vmax) Vmax = Va6

    Vlimit = 0.95 * Vmax

    ! Only Va4 is low
    IF ((Va4 < Vlimit) .AND. (Va5 >= Vlimit) .AND. (Va6 >= Vlimit) .AND. (Ican > 0.0)) THEN
        Sa41_cal = 1.0
        Sa52_cal = 1.0
        Sa62_cal = 1.0

    ! Only Va5 is low
    ELSE IF ((Va5 < Vlimit) .AND. (Va4 >= Vlimit) .AND. (Va6 >= Vlimit) .AND. (Ican > 0.0)) THEN
        Sa42_cal = 1.0
        Sa51_cal = 1.0
        Sa62_cal = 1.0

    ! Only Va6 is low
    ELSE IF ((Va6 < Vlimit) .AND. (Va4 >= Vlimit) .AND. (Va5 >= Vlimit) .AND. (Ican > 0.0)) THEN
        Sa42_cal = 1.0
        Sa52_cal = 1.0
        Sa61_cal = 1.0

    ! Va4 & Va5 are low
    ELSE IF ((Va4 < Vlimit) .AND. (Va5 < Vlimit) .AND. (Va6 >= Vlimit) .AND. (Ican > 0.0)) THEN
        Sa41_cal = 1.0
        Sa51_cal = 1.0
        Sa62_cal = 1.0

    ! Va4 & Va6 are low
    ELSE IF ((Va4 < Vlimit) .AND. (Va6 < Vlimit) .AND. (Va5 >= Vlimit) .AND. (Ican > 0.0)) THEN
        Sa41_cal = 1.0
        Sa52_cal = 1.0
        Sa61_cal = 1.0

    ! Va5 & Va6 are low
    ELSE IF ((Va5 < Vlimit) .AND. (Va6 < Vlimit) .AND. (Va4 >= Vlimit) .AND. (Ican > 0.0)) THEN
        Sa42_cal = 1.0
        Sa51_cal = 1.0
        Sa61_cal = 1.0

    ! All three are low or balanced
    ELSE IF (Ican > 0.0) THEN
        Sa41_cal = 1.0
        Sa51_cal = 1.0
        Sa61_cal = 1.0

    END IF

    !==========================
    ! B PHASE - UPPER ARM
    !==========================

    Vmax = Vb1
    IF (Vb2 > Vmax) Vmax = Vb2
    IF (Vb3 > Vmax) Vmax = Vb3

    Vlimit = 0.95 * Vmax

    ! Only Vb1 is low
    IF ((Vb1 < Vlimit) .AND. (Vb2 >= Vlimit) .AND. (Vb3 >= Vlimit) .AND. (Icbp > 0.0)) THEN
        Sb11_cal = 1.0
        Sb22_cal = 1.0
        Sb32_cal = 1.0

    ! Only Vb2 is low
    ELSE IF ((Vb2 < Vlimit) .AND. (Vb1 >= Vlimit) .AND. (Vb3 >= Vlimit) .AND. (Icbp > 0.0)) THEN
        Sb12_cal = 1.0
        Sb21_cal = 1.0
        Sb32_cal = 1.0

    ! Only Vb3 is low
    ELSE IF ((Vb3 < Vlimit) .AND. (Vb1 >= Vlimit) .AND. (Vb2 >= Vlimit) .AND. (Icbp > 0.0)) THEN
        Sb12_cal = 1.0
        Sb22_cal = 1.0
        Sb31_cal = 1.0

    ! Vb1 & Vb2 are low
    ELSE IF ((Vb1 < Vlimit) .AND. (Vb2 < Vlimit) .AND. (Vb3 >= Vlimit) .AND. (Icbp > 0.0)) THEN
        Sb11_cal = 1.0
        Sb21_cal = 1.0
        Sb32_cal = 1.0

    ! Vb1 & Vb3 are low
    ELSE IF ((Vb1 < Vlimit) .AND. (Vb3 < Vlimit) .AND. (Vb2 >= Vlimit) .AND. (Icbp > 0.0)) THEN
        Sb11_cal = 1.0
        Sb22_cal = 1.0
        Sb31_cal = 1.0

    ! Vb2 & Vb3 are low
    ELSE IF ((Vb2 < Vlimit) .AND. (Vb3 < Vlimit) .AND. (Vb1 >= Vlimit) .AND. (Icbp > 0.0)) THEN
        Sb12_cal = 1.0
        Sb21_cal = 1.0
        Sb31_cal = 1.0

    ! All three are low or balanced
    ELSE IF (Icbp > 0.0) THEN
        Sb11_cal = 1.0
        Sb21_cal = 1.0
        Sb31_cal = 1.0

    END IF

    !==========================
    ! B PHASE - LOWER ARM
    !==========================

    Vmax = Vb4
    IF (Vb5 > Vmax) Vmax = Vb5
    IF (Vb6 > Vmax) Vmax = Vb6

    Vlimit = 0.95 * Vmax

    ! Only Vb4 is low
    IF ((Vb4 < Vlimit) .AND. (Vb5 >= Vlimit) .AND. (Vb6 >= Vlimit) .AND. (Icbn > 0.0)) THEN
        Sb41_cal = 1.0
        Sb52_cal = 1.0
        Sb62_cal = 1.0

    ! Only Vb5 is low
    ELSE IF ((Vb5 < Vlimit) .AND. (Vb4 >= Vlimit) .AND. (Vb6 >= Vlimit) .AND. (Icbn > 0.0)) THEN
        Sb42_cal = 1.0
        Sb51_cal = 1.0
        Sb62_cal = 1.0

    ! Only Vb6 is low
    ELSE IF ((Vb6 < Vlimit) .AND. (Vb4 >= Vlimit) .AND. (Vb5 >= Vlimit) .AND. (Icbn > 0.0)) THEN
        Sb42_cal = 1.0
        Sb52_cal = 1.0
        Sb61_cal = 1.0

    ! Vb4 & Vb5 are low
    ELSE IF ((Vb4 < Vlimit) .AND. (Vb5 < Vlimit) .AND. (Vb6 >= Vlimit) .AND. (Icbn > 0.0)) THEN
        Sb41_cal = 1.0
        Sb51_cal = 1.0
        Sb62_cal = 1.0

    ! Vb4 & Vb6 are low
    ELSE IF ((Vb4 < Vlimit) .AND. (Vb6 < Vlimit) .AND. (Vb5 >= Vlimit) .AND. (Icbn > 0.0)) THEN
        Sb41_cal = 1.0
        Sb52_cal = 1.0
        Sb61_cal = 1.0

    ! Vb5 & Vb6 are low
    ELSE IF ((Vb5 < Vlimit) .AND. (Vb6 < Vlimit) .AND. (Vb4 >= Vlimit) .AND. (Icbn > 0.0)) THEN
        Sb42_cal = 1.0
        Sb51_cal = 1.0
        Sb61_cal = 1.0

    ! All three are low or balanced
    ELSE IF (Icbn > 0.0) THEN
        Sb41_cal = 1.0
        Sb51_cal = 1.0
        Sb61_cal = 1.0

    END IF

    !==========================
    ! C PHASE - UPPER ARM
    !==========================

    Vmax = Vc1
    IF (Vc2 > Vmax) Vmax = Vc2
    IF (Vc3 > Vmax) Vmax = Vc3

    Vlimit = 0.95 * Vmax

    ! Only Vc1 is low
    IF ((Vc1 < Vlimit) .AND. (Vc2 >= Vlimit) .AND. (Vc3 >= Vlimit) .AND. (Iccp > 0.0)) THEN
        Sc11_cal = 1.0
        Sc22_cal = 1.0
        Sc32_cal = 1.0

    ! Only Vc2 is low
    ELSE IF ((Vc2 < Vlimit) .AND. (Vc1 >= Vlimit) .AND. (Vc3 >= Vlimit) .AND. (Iccp > 0.0)) THEN
        Sc12_cal = 1.0
        Sc21_cal = 1.0
        Sc32_cal = 1.0

    ! Only Vc3 is low
    ELSE IF ((Vc3 < Vlimit) .AND. (Vc1 >= Vlimit) .AND. (Vc2 >= Vlimit) .AND. (Iccp > 0.0)) THEN
        Sc12_cal = 1.0
        Sc22_cal = 1.0
        Sc31_cal = 1.0

    ! Vc1 & Vc2 are low
    ELSE IF ((Vc1 < Vlimit) .AND. (Vc2 < Vlimit) .AND. (Vc3 >= Vlimit) .AND. (Iccp > 0.0)) THEN
        Sc11_cal = 1.0
        Sc21_cal = 1.0
        Sc32_cal = 1.0

    ! Vc1 & Vc3 are low
    ELSE IF ((Vc1 < Vlimit) .AND. (Vc3 < Vlimit) .AND. (Vc2 >= Vlimit) .AND. (Iccp > 0.0)) THEN
        Sc11_cal = 1.0
        Sc22_cal = 1.0
        Sc31_cal = 1.0

    ! Vc2 & Vc3 are low
    ELSE IF ((Vc2 < Vlimit) .AND. (Vc3 < Vlimit) .AND. (Vc1 >= Vlimit) .AND. (Iccp > 0.0)) THEN
        Sc12_cal = 1.0
        Sc21_cal = 1.0
        Sc31_cal = 1.0

    ! All three are low or balanced
    ELSE IF (Iccp > 0.0) THEN
        Sc11_cal = 1.0
        Sc21_cal = 1.0
        Sc31_cal = 1.0

    END IF

    !==========================
    ! C PHASE - LOWER ARM
    !==========================

    Vmax = Vc4
    IF (Vc5 > Vmax) Vmax = Vc5
    IF (Vc6 > Vmax) Vmax = Vc6

    Vlimit = 0.95 * Vmax

    ! Only Vc4 is low
    IF ((Vc4 < Vlimit) .AND. (Vc5 >= Vlimit) .AND. (Vc6 >= Vlimit) .AND. (Iccn > 0.0)) THEN
        Sc41_cal = 1.0
        Sc52_cal = 1.0
        Sc62_cal = 1.0

    ! Only Vc5 is low
    ELSE IF ((Vc5 < Vlimit) .AND. (Vc4 >= Vlimit) .AND. (Vc6 >= Vlimit) .AND. (Iccn > 0.0)) THEN
        Sc42_cal = 1.0
        Sc51_cal = 1.0
        Sc62_cal = 1.0

    ! Only Vc6 is low
    ELSE IF ((Vc6 < Vlimit) .AND. (Vc4 >= Vlimit) .AND. (Vc5 >= Vlimit) .AND. (Iccn > 0.0)) THEN
        Sc42_cal = 1.0
        Sc52_cal = 1.0
        Sc61_cal = 1.0

    ! Vc4 & Vc5 are low
    ELSE IF ((Vc4 < Vlimit) .AND. (Vc5 < Vlimit) .AND. (Vc6 >= Vlimit) .AND. (Iccn > 0.0)) THEN
        Sc41_cal = 1.0
        Sc51_cal = 1.0
        Sc62_cal = 1.0

    ! Vc4 & Vc6 are low
    ELSE IF ((Vc4 < Vlimit) .AND. (Vc6 < Vlimit) .AND. (Vc5 >= Vlimit) .AND. (Iccn > 0.0)) THEN
        Sc41_cal = 1.0
        Sc52_cal = 1.0
        Sc61_cal = 1.0

    ! Vc5 & Vc6 are low
    ELSE IF ((Vc5 < Vlimit) .AND. (Vc6 < Vlimit) .AND. (Vc4 >= Vlimit) .AND. (Iccn > 0.0)) THEN
        Sc42_cal = 1.0
        Sc51_cal = 1.0
        Sc61_cal = 1.0

    ! All three are low or balanced
    ELSE IF (Iccn > 0.0) THEN
        Sc41_cal = 1.0
        Sc51_cal = 1.0
        Sc61_cal = 1.0

    END IF

! ======================================================
!  Normal Operation
! ======================================================

ELSE

    ! Close breakers

    Brk_AC = 1
    Brk_DC = 1

    ! ======================================================
    !  Internal Phase Balancing
    ! ======================================================

    ! Phase energy differences

    Eeap = (Va1 + Va2 + Va3) - Ee
    Eean = (Va4 + Va5 + Va6) - Ee
    Eebp = (Vb1 + Vb2 + Vb3) - Ee
    Eebn = (Vb4 + Vb5 + Vb6) - Ee
    Eecp = (Vc1 + Vc2 + Vc3) - Ee
    Eecn = (Vc4 + Vc5 + Vc6) - Ee

    dir_ap = Eeap * Icap
    dir_an = Eean * Ican
    dir_bp = Eebp * Icbp
    dir_bn = Eebn * Icbn
    dir_cp = Eecp * Iccp
    dir_cn = Eecn * Iccn

    ! P/N states

    IF (State == 1) THEN
        Type = 2

        VN1 = Va_ref
        VN2 = Vb_ref
        VP1 = Vc_ref

        EN1 = Eean
        EN2 = Eebn
        EP1 = Eecp

        dirN1 = dir_an
        dirN2 = dir_bn
        dirP1 = dir_cp

    ELSE IF (State == 2) THEN
        Type = 1

        VN1 = Vc_ref
        VP1 = Va_ref
        VP2 = Vb_ref

        EN1 = Eecn
        EP1 = Eeap
        EP2 = Eebp

        dirN1 = dir_cn
        dirP1 = dir_ap
        dirP2 = dir_bp

    ELSE IF (State == 3) THEN
        Type = 2

        VN1 = Va_ref
        VN2 = Vc_ref
        VP1 = Vb_ref

        EN1 = Eean
        EN2 = Eecn
        EP1 = Eebp

        dirN1 = dir_an
        dirN2 = dir_cn
        dirP1 = dir_bp

    ELSE IF (State == 4) THEN
        Type = 1

        VN1 = Vb_ref
        VP1 = Va_ref
        VP2 = Vc_ref

        EN1 = Eebn
        EP1 = Eeap
        EP2 = Eecp

        dirN1 = dir_bn
        dirP1 = dir_ap
        dirP2 = dir_cp

    ELSE IF (State == 5) THEN
        Type = 2

        VN1 = Vb_ref
        VN2 = Vc_ref
        VP1 = Va_ref

        EN1 = Eebn
        EN2 = Eecn
        EP1 = Eeap

        dirN1 = dir_bn
        dirN2 = dir_cn
        dirP1 = dir_ap

    ELSE IF (State == 6) THEN
        Type = 1

        VN1 = Va_ref
        VP1 = Vb_ref
        VP2 = Vc_ref

        EN1 = Eean
        EP1 = Eebp
        EP2 = Eecp

        dirN1 = dir_an
        dirP1 = dir_bp
        dirP2 = dir_cp

    END IF

    !==========================================================
    !  Common Vmid_ref Calculation
    !==========================================================

    IF (Type == 2) THEN
        ! Weights

        wN1 = ABS(EN1)
        wN2 = ABS(EN2)
        wP1 = ABS(EP1)

        ! Limits

        IF (VN1 < VN2) THEN
            Vupper = VN1
        ELSE
            Vupper = VN2
        END IF

        Vlower = VP1

        ! Desired Vmid_ref

        IF (dirN1 > 0.0) THEN
            VdesN1 = VN1
        ELSE
            VdesN1 = Vlower
        END IF

        IF (dirN2 > 0.0) THEN
            VdesN2 = VN2
        ELSE
            VdesN2 = Vlower
        END IF

        IF (dirP1 > 0.0) THEN
            VdesP1 = Vlower
        ELSE
            VdesP1 = Vupper
        END IF

        sum_w = wN1 + wN2 + wP1

        IF (sum_w > 1.0d-6) THEN
            Vmid_ref = (wN1 * VdesN1 + wN2 * VdesN2 + wP1 * VdesP1) / sum_w
        ELSE
            Vmid_ref = 0.5 * (Vupper + Vlower)
        END IF

    ELSE
        ! Weights

        wN1 = ABS(EN1)
        wP1 = ABS(EP1)
        wP2 = ABS(EP2)

        ! Limits

        IF (VP1 > VP2) THEN
            Vlower = VP1
        ELSE
            Vlower = VP2
        END IF

        Vupper = VN1

        ! Desired Vmid_ref

        IF (dirN1 > 0.0) THEN
            VdesN1 = VN1
        ELSE
            VdesN1 = Vlower
        END IF

        IF (dirP1 > 0.0) THEN
            VdesP1 = Vlower
        ELSE
            VdesP1 = Vupper
        END IF

        IF (dirP2 > 0.0) THEN
            VdesP2 = Vlower
        ELSE
            VdesP2 = Vupper
        END IF

        sum_w = wN1 + wP1 + wP2

        IF (sum_w > 1.0d-6) THEN
            Vmid_ref = (wN1 * VdesN1 + wP1 * VdesP1 + wP2 * VdesP2) / sum_w
        ELSE
            Vmid_ref = 0.5 * (Vupper + Vlower)
        END IF

    END IF

    ! Saturation

    IF (Vmid_ref < Vlower) Vmid_ref = Vlower

    IF (Vmid_ref > Vupper) Vmid_ref = Vupper

    ! Z states

    IF ((State == 7) .OR. (State == 8)) THEN
        Vmid_ref = Va_ref

    ELSE IF ((State == 9) .OR. (State == 10)) THEN
        Vmid_ref = Vb_ref

    ELSE IF ((State == 11) .OR. (State == 12)) THEN
        Vmid_ref = Vc_ref

    END IF

    ! ======================================================
    !  Converter State Machine
    ! ======================================================

    SELECT CASE (State)

    CASE (1)

        Vap_ref = Vdc_ref - Va_ref
        Van_ref = Va_ref - Vmid_ref

        Vbp_ref = Vdc_ref - Vb_ref
        Vbn_ref = Vb_ref - Vmid_ref

        Vcp_ref = -Vc_ref + Vmid_ref
        Vcn_ref = Vc_ref + Vdc_ref


    CASE (2)

        Vap_ref = -Va_ref + Vmid_ref
        Van_ref = Va_ref + Vdc_ref

        Vbp_ref = -Vb_ref + Vmid_ref
        Vbn_ref = Vb_ref + Vdc_ref

        Vcp_ref = Vdc_ref - Vc_ref
        Vcn_ref = Vc_ref - Vmid_ref


    CASE (3)

        Vap_ref = Vdc_ref - Va_ref
        Van_ref = Va_ref - Vmid_ref

        Vbp_ref = -Vb_ref + Vmid_ref
        Vbn_ref = Vb_ref + Vdc_ref

        Vcp_ref = Vdc_ref - Vc_ref
        Vcn_ref = Vc_ref - Vmid_ref


    CASE (4)

        Vap_ref = -Va_ref + Vmid_ref
        Van_ref = Va_ref + Vdc_ref

        Vbp_ref = Vdc_ref - Vb_ref
        Vbn_ref = Vb_ref - Vmid_ref

        Vcp_ref = -Vc_ref + Vmid_ref
        Vcn_ref = Vc_ref + Vdc_ref


    CASE (5)

        Vap_ref = -Va_ref + Vmid_ref
        Van_ref = Va_ref + Vdc_ref

        Vbp_ref = Vdc_ref - Vb_ref
        Vbn_ref = Vb_ref - Vmid_ref

        Vcp_ref = Vdc_ref - Vc_ref
        Vcn_ref = Vc_ref - Vmid_ref


    CASE (6)

        Vap_ref = Vdc_ref - Va_ref
        Van_ref = Va_ref - Vmid_ref

        Vbp_ref = -Vb_ref + Vmid_ref
        Vbn_ref = Vb_ref + Vdc_ref

        Vcp_ref = -Vc_ref + Vmid_ref
        Vcn_ref = Vc_ref + Vdc_ref


    CASE (7)

        Vap_ref = -Va_ref + Vmid_ref
        Van_ref = Va_ref - Vmid_ref

        Vbp_ref = Vdc_ref - Vb_ref
        Vbn_ref = Vb_ref - Vmid_ref

        Vcp_ref = -Vc_ref + Vmid_ref
        Vcn_ref = Vc_ref + Vdc_ref


    CASE (8)

        Vap_ref = -Va_ref + Vmid_ref
        Van_ref = Va_ref - Vmid_ref

        Vbp_ref = -Vb_ref + Vmid_ref
        Vbn_ref = Vb_ref + Vdc_ref

        Vcp_ref = Vdc_ref - Vc_ref
        Vcn_ref = Vc_ref - Vmid_ref


    CASE (9)

        Vap_ref = Vdc_ref - Va_ref
        Van_ref = Va_ref - Vmid_ref

        Vbp_ref = -Vb_ref + Vmid_ref
        Vbn_ref = Vb_ref - Vmid_ref

        Vcp_ref = -Vc_ref + Vmid_ref
        Vcn_ref = Vc_ref + Vdc_ref


    CASE (10)

        Vap_ref = -Va_ref + Vmid_ref
        Van_ref = Va_ref + Vdc_ref

        Vbp_ref = -Vb_ref + Vmid_ref
        Vbn_ref = Vb_ref - Vmid_ref

        Vcp_ref = Vdc_ref - Vc_ref
        Vcn_ref = Vc_ref - Vmid_ref


    CASE (11)

        Vap_ref = Vdc_ref - Va_ref
        Van_ref = Va_ref - Vmid_ref

        Vbp_ref = -Vb_ref + Vmid_ref
        Vbn_ref = Vb_ref + Vdc_ref

        Vcp_ref = -Vc_ref + Vmid_ref
        Vcn_ref = Vc_ref - Vmid_ref


    CASE (12)

        Vap_ref = -Va_ref + Vmid_ref
        Van_ref = Va_ref + Vdc_ref

        Vbp_ref = Vdc_ref - Vb_ref
        Vbn_ref = Vb_ref - Vmid_ref

        Vcp_ref = -Vc_ref + Vmid_ref
        Vcn_ref = Vc_ref - Vmid_ref


    CASE DEFAULT

        Vap_ref = 0.0
        Van_ref = 0.0

        Vbp_ref = 0.0
        Vbn_ref = 0.0

        Vcp_ref = 0.0
        Vcn_ref = 0.0

    END SELECT

    ! ======================================================
    !  Switching Signals - Phase A
    ! ======================================================

    IF ((State == 1) .OR. (State == 3) .OR. (State == 6) .OR. &
       (State == 9) .OR. (State == 11)) THEN
        ! P State

        SaT1 = 1
        SaT2 = 0
        SaT3 = 1
        SaT4 = 0

    ELSE IF ((State == 7) .OR. (State == 8)) THEN
        ! Z State

        SaT1 = 0
        SaT2 = 1
        SaT3 = 1
        SaT4 = 0

    ELSE
        ! N State

        SaT1 = 0
        SaT2 = 1
        SaT3 = 0
        SaT4 = 1

    END IF

    ! ======================================================
    !  Switching Signals - Phase B
    ! ======================================================

    IF ((State == 1) .OR. (State == 4) .OR. (State == 5) .OR. &
       (State == 7) .OR. (State == 12)) THEN
        ! P State

        SbT1 = 1
        SbT2 = 0
        SbT3 = 1
        SbT4 = 0

    ELSE IF ((State == 9) .OR. (State == 10)) THEN
        ! Z State

        SbT1 = 0
        SbT2 = 1
        SbT3 = 1
        SbT4 = 0

    ELSE
        ! N State

        SbT1 = 0
        SbT2 = 1
        SbT3 = 0
        SbT4 = 1

    END IF

    ! ======================================================
    !  Switching Signals - Phase C
    ! ======================================================

    IF ((State == 2) .OR. (State == 3) .OR. (State == 5) .OR. &
       (State == 8) .OR. (State == 10)) THEN
        ! P State

        ScT1 = 1
        ScT2 = 0
        ScT3 = 1
        ScT4 = 0

    ELSE IF ((State == 11) .OR. (State == 12)) THEN
        ! Z State

        ScT1 = 0
        ScT2 = 1
        ScT3 = 1
        ScT4 = 0

    ELSE
        ! N State

        ScT1 = 0
        ScT2 = 1
        ScT3 = 0
        ScT4 = 1

    END IF

    ! --------------------------------------------------
    !  Determine inserted SM numbers
    ! --------------------------------------------------

    numap = 0
    IF (Vap_ref >= 2*Vlevel) THEN
        numap = 3
    ELSE IF (Vap_ref >= Vlevel) THEN
        numap = 2
    ELSE IF (Vap_ref >= 0.0) THEN
        numap = 1
    END IF

    numan = 0
    IF (Van_ref >= 2*Vlevel) THEN
        numan = 3
    ELSE IF (Van_ref >= Vlevel) THEN
        numan = 2
    ELSE IF (Van_ref >= 0.0) THEN
        numan = 1
    END IF

    numbp = 0
    IF (Vbp_ref >= 2*Vlevel) THEN
        numbp = 3
    ELSE IF (Vbp_ref >= Vlevel) THEN
        numbp = 2
    ELSE IF (Vbp_ref >= 0.0) THEN
        numbp = 1
    END IF

    numbn = 0
    IF (Vbn_ref >= 2*Vlevel) THEN
        numbn = 3
    ELSE IF (Vbn_ref >= Vlevel) THEN
        numbn = 2
    ELSE IF (Vbn_ref >= 0.0) THEN
        numbn = 1
    END IF

    numcp = 0
    IF (Vcp_ref >= 2*Vlevel) THEN
        numcp = 3
    ELSE IF (Vcp_ref >= Vlevel) THEN
        numcp = 2
    ELSE IF (Vcp_ref >= 0.0) THEN
        numcp = 1
    END IF

    numcn = 0
    IF (Vcn_ref >= 2*Vlevel) THEN
        numcn = 3
    ELSE IF (Vcn_ref >= Vlevel) THEN
        numcn = 2
    ELSE IF (Vcn_ref >= 0.0) THEN
        numcn = 1
    END IF

    !====================================================
    !                    PHASE A
    !====================================================

    !------------------ Upper arm A ------------------

    base = numap - 1

    IF (base < 0) base = 0

    IF (base > 2) base = 2

    !---------------- Base inserted SM ----------------

    IF (base == 1) THEN
        IF (Icap > 0.0) THEN
            IF (Va2 <= Va3) THEN
                Sa21_cal = 1.0
            ELSE
                Sa31_cal = 1.0
            END IF
        ELSE
            IF (Va2 >= Va3) THEN
                Sa21_cal = 1.0
            ELSE
                Sa31_cal = 1.0
            END IF
        END IF
    ELSE IF (base == 2) THEN
        Sa21_cal = 1.0
        Sa31_cal = 1.0
    END IF

    !---------------- PWM residual ----------------

    pwm_flag = 0

    IF (numap == 1) THEN
        IF ((Vlevel * PWM) < Vap_ref) pwm_flag = 1
    ELSE IF (numap == 2) THEN
        IF (((Vlevel * PWM) + Vlevel) < Vap_ref) pwm_flag = 1
    ELSE IF (numap == 3) THEN
        IF (((Vlevel * PWM) + 2*Vlevel) < Vap_ref) pwm_flag = 1
    END IF

    !---------------- SiC enable ----------------

    VSI_AVG = 0.5 * (Va2 + Va3)
    VDIFF   = Va1 - VSI_AVG

    sic_en = 1

    IF (VDIFF > 0.05*Vlevel) THEN
        IF (Icap > 0.0) sic_en = 0
    ELSE IF (VDIFF < -0.05*Vlevel) THEN
        IF (Icap < 0.0) sic_en = 0
    END IF

    IF (numap == 3) sic_en = 1

    !---------------- PWM assignment ----------------

    IF (pwm_flag == 1) THEN
        IF (sic_en == 1) THEN
            Sa11_cal = 1.0
        ELSE
            IF (base == 0) THEN
                IF (Icap > 0.0) THEN
                    IF (Va2 <= Va3) THEN
                        Sa21_cal = 1.0
                    ELSE
                        Sa31_cal = 1.0
                    END IF
                ELSE
                    IF (Va2 >= Va3) THEN
                        Sa21_cal = 1.0
                    ELSE
                        Sa31_cal = 1.0
                    END IF
                END IF
            ELSE IF (base == 1) THEN
                IF (Sa21_cal == 1.0) THEN
                    Sa31_cal = 1.0
                ELSE
                    Sa21_cal = 1.0
                END IF
            ELSE
                Sa11_cal = 1.0
            END IF
        END IF
    END IF

    IF ((Va1 > max_cap_voltage) .AND. (Icap > 0.0)) THEN
        Sa11_cal = 0.0
    END IF
    IF ((Va2 > max_cap_voltage) .AND. (Icap > 0.0)) THEN
        Sa21_cal = 0.0
    END IF
    IF ((Va3 > max_cap_voltage) .AND. (Icap > 0.0)) THEN
        Sa31_cal = 0.0
    END IF

    !------------------ Lower arm A ------------------

    base = numan - 1

    IF (base < 0) base = 0

    IF (base > 2) base = 2

    !---------------- Base inserted SM ----------------

    IF (base == 1) THEN
        IF (Ican > 0.0) THEN
            IF (Va4 <= Va5) THEN
                Sa41_cal = 1.0
            ELSE
                Sa51_cal = 1.0
            END IF
        ELSE
            IF (Va4 >= Va5) THEN
                Sa41_cal = 1.0
            ELSE
                Sa51_cal = 1.0
            END IF
        END IF
    ELSE IF (base == 2) THEN
        Sa41_cal = 1.0
        Sa51_cal = 1.0
    END IF

    !---------------- PWM residual ----------------

    pwm_flag = 0

    IF (numan == 1) THEN
        IF ((Vlevel * PWM) < Van_ref) pwm_flag = 1
    ELSE IF (numan == 2) THEN
        IF (((Vlevel * PWM) + Vlevel) < Van_ref) pwm_flag = 1
    ELSE IF (numan == 3) THEN
        IF (((Vlevel * PWM) + 2*Vlevel) < Van_ref) pwm_flag = 1
    END IF

    !---------------- SiC enable ----------------

    VSI_AVG = 0.5 * (Va4 + Va5)
    VDIFF   = Va6 - VSI_AVG

    sic_en = 1

    IF (VDIFF > 0.05*Vlevel) THEN
        IF (Ican > 0.0) sic_en = 0
    ELSE IF (VDIFF < -0.05*Vlevel) THEN
        IF (Ican < 0.0) sic_en = 0
    END IF

    IF (numan == 3) sic_en = 1

    !---------------- PWM assignment ----------------

    IF (pwm_flag == 1) THEN
        IF (sic_en == 1) THEN
            Sa61_cal = 1.0
        ELSE
            IF (base == 0) THEN
                IF (Ican > 0.0) THEN
                    IF (Va4 <= Va5) THEN
                        Sa41_cal = 1.0
                    ELSE
                        Sa51_cal = 1.0
                    END IF
                ELSE
                    IF (Va4 >= Va5) THEN
                        Sa41_cal = 1.0
                    ELSE
                        Sa51_cal = 1.0
                    END IF
                END IF
            ELSE IF (base == 1) THEN
                IF (Sa41_cal == 1.0) THEN
                    Sa51_cal = 1.0
                ELSE
                    Sa41_cal = 1.0
                END IF
            ELSE
                Sa61_cal = 1.0
            END IF
        END IF
    END IF
    IF ((Va4 > max_cap_voltage) .AND. (Ican > 0.0)) THEN
        Sa41_cal = 0.0
    END IF
    IF ((Va5 > max_cap_voltage) .AND. (Ican > 0.0)) THEN
        Sa51_cal = 0.0
    END IF
    IF ((Va6 > max_cap_voltage) .AND. (Ican > 0.0)) THEN
        Sa61_cal = 0.0
    END IF

    !====================================================
    !                    PHASE B
    !====================================================

    !------------------ Upper arm B ------------------

    base = numbp - 1

    IF (base < 0) base = 0

    IF (base > 2) base = 2

    !---------------- Base inserted SM ----------------

    IF (base == 1) THEN
        IF (Icbp > 0.0) THEN
            IF (Vb2 <= Vb3) THEN
                Sb21_cal = 1.0
            ELSE
                Sb31_cal = 1.0
            END IF
        ELSE
            IF (Vb2 >= Vb3) THEN
                Sb21_cal = 1.0
            ELSE
                Sb31_cal = 1.0
            END IF
        END IF
    ELSE IF (base == 2) THEN
        Sb21_cal = 1.0
        Sb31_cal = 1.0
    END IF

    !---------------- PWM residual ----------------

    pwm_flag = 0

    IF (numbp == 1) THEN
        IF ((Vlevel * PWM) < Vbp_ref) pwm_flag = 1
    ELSE IF (numbp == 2) THEN
        IF (((Vlevel * PWM) + Vlevel) < Vbp_ref) pwm_flag = 1
    ELSE IF (numbp == 3) THEN
        IF (((Vlevel * PWM) + 2*Vlevel) < Vbp_ref) pwm_flag = 1
    END IF

    !---------------- SiC enable ----------------

    VSI_AVG = 0.5 * (Vb2 + Vb3)
    VDIFF   = Vb1 - VSI_AVG

    sic_en = 1

    IF (VDIFF > 0.05*Vlevel) THEN
        IF (Icbp > 0.0) sic_en = 0
    ELSE IF (VDIFF < -0.05*Vlevel) THEN
        IF (Icbp < 0.0) sic_en = 0
    END IF

    IF (numbp == 3) sic_en = 1

    !---------------- PWM assignment ----------------

    IF (pwm_flag == 1) THEN
        IF (sic_en == 1) THEN
            Sb11_cal = 1.0
        ELSE
            IF (base == 0) THEN
                IF (Icbp > 0.0) THEN
                    IF (Vb2 <= Vb3) THEN
                        Sb21_cal = 1.0
                    ELSE
                        Sb31_cal = 1.0
                    END IF
                ELSE
                    IF (Vb2 >= Vb3) THEN
                        Sb21_cal = 1.0
                    ELSE
                        Sb31_cal = 1.0
                    END IF
                END IF
            ELSE IF (base == 1) THEN
                IF (Sb21_cal == 1.0) THEN
                    Sb31_cal = 1.0
                ELSE
                    Sb21_cal = 1.0
                END IF
            ELSE
                Sb11_cal = 1.0
            END IF
        END IF
    END IF
    IF ((Vb1 > max_cap_voltage) .AND. (Icbp > 0.0)) THEN
        Sb11_cal = 0.0
    END IF
    IF ((Vb2 > max_cap_voltage) .AND. (Icbp > 0.0)) THEN
        Sb21_cal = 0.0
    END IF
    IF ((Vb3 > max_cap_voltage) .AND. (Icbp > 0.0)) THEN
        Sb31_cal = 0.0
    END IF

    !------------------ Lower arm B ------------------

    base = numbn - 1

    IF (base < 0) base = 0

    IF (base > 2) base = 2

    !---------------- Base inserted SM ----------------

    IF (base == 1) THEN
        IF (Icbn > 0.0) THEN
            IF (Vb4 <= Vb5) THEN
                Sb41_cal = 1.0
            ELSE
                Sb51_cal = 1.0
            END IF
        ELSE
            IF (Vb4 >= Vb5) THEN
                Sb41_cal = 1.0
            ELSE
                Sb51_cal = 1.0
            END IF
        END IF
    ELSE IF (base == 2) THEN
        Sb41_cal = 1.0
        Sb51_cal = 1.0
    END IF

    !---------------- PWM residual ----------------

    pwm_flag = 0

    IF (numbn == 1) THEN
        IF ((Vlevel * PWM) < Vbn_ref) pwm_flag = 1
    ELSE IF (numbn == 2) THEN
        IF (((Vlevel * PWM) + Vlevel) < Vbn_ref) pwm_flag = 1
    ELSE IF (numbn == 3) THEN
        IF (((Vlevel * PWM) + 2*Vlevel) < Vbn_ref) pwm_flag = 1
    END IF

    !---------------- SiC enable ----------------

    VSI_AVG = 0.5 * (Vb4 + Vb5)
    VDIFF   = Vb6 - VSI_AVG

    sic_en = 1

    IF (VDIFF > 0.05*Vlevel) THEN
        IF (Icbn > 0.0) sic_en = 0
    ELSE IF (VDIFF < -0.05*Vlevel) THEN
        IF (Icbn < 0.0) sic_en = 0
    END IF

    IF (numbn == 3) sic_en = 1

    !---------------- PWM assignment ----------------

    IF (pwm_flag == 1) THEN
        IF (sic_en == 1) THEN
            Sb61_cal = 1.0
        ELSE
            IF (base == 0) THEN
                IF (Icbn > 0.0) THEN
                    IF (Vb4 <= Vb5) THEN
                        Sb41_cal = 1.0
                    ELSE
                        Sb51_cal = 1.0
                    END IF
                ELSE
                    IF (Vb4 >= Vb5) THEN
                        Sb41_cal = 1.0
                    ELSE
                        Sb51_cal = 1.0
                    END IF
                END IF
            ELSE IF (base == 1) THEN
                IF (Sb41_cal == 1.0) THEN
                    Sb51_cal = 1.0
                ELSE
                    Sb41_cal = 1.0
                END IF
            ELSE
                Sb61_cal = 1.0
            END IF
        END IF
    END IF
    IF ((Vb4 > max_cap_voltage) .AND. (Icbn > 0.0)) THEN
        Sb41_cal = 0.0
    END IF
    IF ((Vb5 > max_cap_voltage) .AND. (Icbn > 0.0)) THEN
        Sb51_cal = 0.0
    END IF
    IF ((Vb6 > max_cap_voltage) .AND. (Icbn > 0.0)) THEN
        Sb61_cal = 0.0
    END IF

    !====================================================
    !                    PHASE C
    !====================================================

    !------------------ Upper arm C ------------------

    base = numcp - 1

    IF (base < 0) base = 0

    IF (base > 2) base = 2

    !---------------- Base inserted SM ----------------

    IF (base == 1) THEN
        IF (Iccp > 0.0) THEN
            IF (Vc2 <= Vc3) THEN
                Sc21_cal = 1.0
            ELSE
                Sc31_cal = 1.0
            END IF
        ELSE
            IF (Vc2 >= Vc3) THEN
                Sc21_cal = 1.0
            ELSE
                Sc31_cal = 1.0
            END IF
        END IF
    ELSE IF (base == 2) THEN
        Sc21_cal = 1.0
        Sc31_cal = 1.0
    END IF

    !---------------- PWM residual ----------------

    pwm_flag = 0

    IF (numcp == 1) THEN
        IF ((Vlevel * PWM) < Vcp_ref) pwm_flag = 1
    ELSE IF (numcp == 2) THEN
        IF (((Vlevel * PWM) + Vlevel) < Vcp_ref) pwm_flag = 1
    ELSE IF (numcp == 3) THEN
        IF (((Vlevel * PWM) + 2*Vlevel) < Vcp_ref) pwm_flag = 1
    END IF

    !---------------- SiC enable ----------------

    VSI_AVG = 0.5 * (Vc2 + Vc3)
    VDIFF   = Vc1 - VSI_AVG

    sic_en = 1

    IF (VDIFF > 0.05*Vlevel) THEN
        IF (Iccp > 0.0) sic_en = 0
    ELSE IF (VDIFF < -0.05*Vlevel) THEN
        IF (Iccp < 0.0) sic_en = 0
    END IF

    IF (numcp == 3) sic_en = 1

    !---------------- PWM assignment ----------------

    IF (pwm_flag == 1) THEN
        IF (sic_en == 1) THEN
            Sc11_cal = 1.0
        ELSE
            IF (base == 0) THEN
                IF (Iccp > 0.0) THEN
                    IF (Vc2 <= Vc3) THEN
                        Sc21_cal = 1.0
                    ELSE
                        Sc31_cal = 1.0
                    END IF
                ELSE
                    IF (Vc2 >= Vc3) THEN
                        Sc21_cal = 1.0
                    ELSE
                        Sc31_cal = 1.0
                    END IF
                END IF
            ELSE IF (base == 1) THEN
                IF (Sc21_cal == 1.0) THEN
                    Sc31_cal = 1.0
                ELSE
                    Sc21_cal = 1.0
                END IF
            ELSE
                Sc11_cal = 1.0
            END IF
        END IF
    END IF
    IF ((Vc1 > max_cap_voltage) .AND. (Iccp > 0.0)) THEN
        Sc11_cal = 0.0
    END IF
    IF ((Vc2 > max_cap_voltage) .AND. (Iccp > 0.0)) THEN
        Sc21_cal = 0.0
    END IF
    IF ((Vc3 > max_cap_voltage) .AND. (Iccp > 0.0)) THEN
        Sc31_cal = 0.0
    END IF

    !------------------ Lower arm C ------------------

    base = numcn - 1

    IF (base < 0) base = 0

    IF (base > 2) base = 2

    !---------------- Base inserted SM ----------------

    IF (base == 1) THEN
        IF (Iccn > 0.0) THEN
            IF (Vc4 <= Vc5) THEN
                Sc41_cal = 1.0
            ELSE
                Sc51_cal = 1.0
            END IF
        ELSE
            IF (Vc4 >= Vc5) THEN
                Sc41_cal = 1.0
            ELSE
                Sc51_cal = 1.0
            END IF
        END IF
    ELSE IF (base == 2) THEN
        Sc41_cal = 1.0
        Sc51_cal = 1.0
    END IF

    !---------------- PWM residual ----------------

    pwm_flag = 0

    IF (numcn == 1) THEN
        IF ((Vlevel * PWM) < Vcn_ref) pwm_flag = 1
    ELSE IF (numcn == 2) THEN
        IF (((Vlevel * PWM) + Vlevel) < Vcn_ref) pwm_flag = 1
    ELSE IF (numcn == 3) THEN
        IF (((Vlevel * PWM) + 2*Vlevel) < Vcn_ref) pwm_flag = 1
    END IF

    !---------------- SiC enable ----------------

    VSI_AVG = 0.5 * (Vc4 + Vc5)
    VDIFF   = Vc6 - VSI_AVG

    sic_en = 1

    IF (VDIFF > 0.05*Vlevel) THEN
        IF (Iccn > 0.0) sic_en = 0
    ELSE IF (VDIFF < -0.05*Vlevel) THEN
        IF (Iccn < 0.0) sic_en = 0
    END IF

    IF (numcn == 3) sic_en = 1

    !---------------- PWM assignment ----------------

    IF (pwm_flag == 1) THEN
        IF (sic_en == 1) THEN
            Sc61_cal = 1.0
        ELSE
            IF (base == 0) THEN
                IF (Iccn > 0.0) THEN
                    IF (Vc4 <= Vc5) THEN
                        Sc41_cal = 1.0
                    ELSE
                        Sc51_cal = 1.0
                    END IF
                ELSE
                    IF (Vc4 >= Vc5) THEN
                        Sc41_cal = 1.0
                    ELSE
                        Sc51_cal = 1.0
                    END IF
                END IF
            ELSE IF (base == 1) THEN
                IF (Sc41_cal == 1.0) THEN
                    Sc51_cal = 1.0
                ELSE
                    Sc41_cal = 1.0
                END IF
            ELSE
                Sc61_cal = 1.0
            END IF
        END IF
    END IF
    IF ((Vc4 > max_cap_voltage) .AND. (Iccn > 0.0)) THEN
        Sc41_cal = 0.0
    END IF
    IF ((Vc5 > max_cap_voltage) .AND. (Iccn > 0.0)) THEN
        Sc51_cal = 0.0
    END IF
    IF ((Vc6 > max_cap_voltage) .AND. (Iccn > 0.0)) THEN
        Sc61_cal = 0.0
    END IF

    !=====================================================
    ! COMPLEMENTARY SWITCH GENERATION
    ! STATE 2 NORMAL OPERATION
    !=====================================================

    ! Phase A
    Sa12_cal = 1.0 - Sa11_cal
    Sa22_cal = 1.0 - Sa21_cal
    Sa32_cal = 1.0 - Sa31_cal
    Sa42_cal = 1.0 - Sa41_cal
    Sa52_cal = 1.0 - Sa51_cal
    Sa62_cal = 1.0 - Sa61_cal

    ! Phase B
    Sb12_cal = 1.0 - Sb11_cal
    Sb22_cal = 1.0 - Sb21_cal
    Sb32_cal = 1.0 - Sb31_cal
    Sb42_cal = 1.0 - Sb41_cal
    Sb52_cal = 1.0 - Sb51_cal
    Sb62_cal = 1.0 - Sb61_cal

    ! Phase C

    Sc12_cal = 1.0 - Sc11_cal
    Sc22_cal = 1.0 - Sc21_cal
    Sc32_cal = 1.0 - Sc31_cal
    Sc42_cal = 1.0 - Sc41_cal
    Sc52_cal = 1.0 - Sc51_cal
    Sc62_cal = 1.0 - Sc61_cal

END IF   ! End of Normal Operation

! --------------------------------------------------
!  Putout switching outputs
! --------------------------------------------------

!---------------- Phase A ----------------

Sa11 = Sa11_cal
Sa12 = Sa12_cal
Sa21 = Sa21_cal
Sa22 = Sa22_cal
Sa31 = Sa31_cal
Sa32 = Sa32_cal
Sa41 = Sa41_cal
Sa42 = Sa42_cal
Sa51 = Sa51_cal
Sa52 = Sa52_cal
Sa61 = Sa61_cal
Sa62 = Sa62_cal

!---------------- Phase B ----------------

Sb11 = Sb11_cal
Sb12 = Sb12_cal
Sb21 = Sb21_cal
Sb22 = Sb22_cal
Sb31 = Sb31_cal
Sb32 = Sb32_cal
Sb41 = Sb41_cal
Sb42 = Sb42_cal
Sb51 = Sb51_cal
Sb52 = Sb52_cal
Sb61 = Sb61_cal
Sb62 = Sb62_cal

!---------------- Phase C ----------------

Sc11 = Sc11_cal
Sc12 = Sc12_cal
Sc21 = Sc21_cal
Sc22 = Sc22_cal
Sc31 = Sc31_cal
Sc32 = Sc32_cal
Sc41 = Sc41_cal
Sc42 = Sc42_cal
Sc51 = Sc51_cal
Sc52 = Sc52_cal
Sc61 = Sc61_cal
Sc62 = Sc62_cal

! ------------ End of CODE: Section -------------
