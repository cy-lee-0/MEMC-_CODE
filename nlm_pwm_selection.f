#LOCAL INTEGER numap
#LOCAL INTEGER numan
#LOCAL INTEGER numbp
#LOCAL INTEGER numbn
#LOCAL INTEGER numcp
#LOCAL INTEGER numcn

#LOCAL REAL lv1
#LOCAL REAL lv2
#LOCAL REAL lv3

#LOCAL REAL vap1
#LOCAL REAL vap2
#LOCAL REAL vap3

#LOCAL REAL van1
#LOCAL REAL van2
#LOCAL REAL van3

#LOCAL INTEGER k

#LOCAL INTEGER Sa_local 6
#LOCAL INTEGER Sb_local 6
#LOCAL INTEGER Sc_local 6

#LOCAL INTEGER base
#LOCAL INTEGER pwm_flag
#LOCAL INTEGER sic_en

#LOCAL REAL VSM_REF
#LOCAL REAL VLOW
#LOCAL REAL VHIGH
#LOCAL REAL VSI_AVG
#LOCAL REAL VDIFF


!------------------ Voltage levels ------------------
lv3 = 375.0
lv2 = 250.0
lv1 = 125.0


!------------------ Capacitor voltage reference and limits ------------------
! VSM_REF : target SM capacitor voltage in pu, 0.125 pu = 125 V
! VLOW    : lower protection limit for SiC-SM voltage
! VHIGH   : upper protection limit for SiC-SM voltage

VSM_REF = 0.125
VLOW    = 0.120
VHIGH   = 0.130


!------------------ Determine inserted SM numbers ------------------

numap = 0
numan = 0
numbp = 0
numbn = 0
numcp = 0
numcn = 0


IF ($Vap_ref >= lv2) THEN
  numap = 3
ELSEIF ($Vap_ref >= lv1) THEN
  numap = 2
ELSEIF ($Vap_ref >= 0.0) THEN
  numap = 1
ENDIF


IF ($Van_ref >= lv2) THEN
  numan = 3
ELSEIF ($Van_ref >= lv1) THEN
  numan = 2
ELSEIF ($Van_ref >= 0.0) THEN
  numan = 1
ENDIF


IF ($Vbp_ref >= lv2) THEN
  numbp = 3
ELSEIF ($Vbp_ref >= lv1) THEN
  numbp = 2
ELSEIF ($Vbp_ref >= 0.0) THEN
  numbp = 1
ENDIF


IF ($Vbn_ref >= lv2) THEN
  numbn = 3
ELSEIF ($Vbn_ref >= lv1) THEN
  numbn = 2
ELSEIF ($Vbn_ref >= 0.0) THEN
  numbn = 1
ENDIF


IF ($Vcp_ref >= lv2) THEN
  numcp = 3
ELSEIF ($Vcp_ref >= lv1) THEN
  numcp = 2
ELSEIF ($Vcp_ref >= 0.0) THEN
  numcp = 1
ENDIF


IF ($Vcn_ref >= lv2) THEN
  numcn = 3
ELSEIF ($Vcn_ref >= lv1) THEN
  numcn = 2
ELSEIF ($Vcn_ref >= 0.0) THEN
  numcn = 1
ENDIF


!------------------ Reset switching ------------------

DO k = 1,6
  Sa_local(k) = 0
  Sb_local(k) = 0
  Sc_local(k) = 0
ENDDO


!====================================================
!                    PHASE A
!====================================================

!------------------ Upper arm A  (SM1 = SiC, SM2/SM3 = Si) ------------------
! Si-SMs perform base NLM. SiC-SM performs PWM residual only when it improves SiC voltage balance.
! If SiC PWM would worsen balancing, PWM is blocked and all three SMs, including SiC, are sorted by NLM.
vap1 = $Vcapsa(1)
vap2 = $Vcapsa(2)
vap3 = $Vcapsa(3)

!------------------ 1) Base NLM insertion using Si-SMs ------------------
base = numap - 1

IF (base < 0) THEN
  base = 0
ENDIF

IF (base > 2) THEN
  base = 2
ENDIF

IF (base == 1) THEN

  IF ($Icma(1) > 0.0) THEN
    IF (vap2 <= vap3) THEN
      Sa_local(2) = 1
    ELSE
      Sa_local(3) = 1
    ENDIF
  ELSE
    IF (vap2 >= vap3) THEN
      Sa_local(2) = 1
    ELSE
      Sa_local(3) = 1
    ENDIF
  ENDIF

ELSEIF (base == 2) THEN

  Sa_local(2) = 1
  Sa_local(3) = 1

ENDIF


!------------------ 2) PWM residual decision ------------------
pwm_flag = 0

IF (numap == 1) THEN
  IF ((lv1*$PWM) < $Vap_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numap == 2) THEN
  IF (((lv1*$PWM) + lv1) < $Vap_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numap == 3) THEN
  IF (((lv1*$PWM) + lv2) < $Vap_ref) THEN
    pwm_flag = 1
  ENDIF
ENDIF


!------------------ 3) SiC PWM enable decision ------------------
! VDIFF > 0 : SiC-SM voltage is higher than Si-SM average.
! VDIFF < 0 : SiC-SM voltage is lower than Si-SM average.
VSI_AVG = 0.5 * (vap2 + vap3)
VDIFF   = vap1 - VSI_AVG

sic_en = 0

IF ($Icma(1) > 0.0) THEN
  ! Insertion is treated as charging direction based on the original Si-SM sorting rule.
  IF ((VDIFF < 0.0) .OR. (vap1 < VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ELSE
  ! Insertion is treated as discharging direction based on the original Si-SM sorting rule.
  IF ((VDIFF > 0.0) .OR. (vap1 > VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ENDIF

! Absolute direction-aware protection for SiC-SM.
IF (vap1 < VLOW) THEN
  IF ($Icma(1) < 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF

IF (vap1 > VHIGH) THEN
  IF ($Icma(1) > 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF


!------------------ 4) PWM/NLM assignment ------------------
IF (pwm_flag == 1) THEN

  IF (sic_en == 1) THEN

    Sa_local(1) = 1

  ELSE

    ! SiC PWM is disabled. Use NLM sorting including the SiC-SM.
    Sa_local(1) = 0
    Sa_local(2) = 0
    Sa_local(3) = 0

    IF ($Vap_ref >= (lv1*2.5)) THEN

      Sa_local(1) = 1
      Sa_local(2) = 1
      Sa_local(3) = 1

    ELSEIF ($Vap_ref >= (lv1*1.5)) THEN

      IF ($Icma(1) > 0.0) THEN
        ! Insert the two lowest-voltage SMs for charging-direction balancing.
        IF ((vap1 >= vap2) .AND. (vap1 >= vap3)) THEN
          Sa_local(2) = 1
          Sa_local(3) = 1
        ELSEIF ((vap2 >= vap1) .AND. (vap2 >= vap3)) THEN
          Sa_local(1) = 1
          Sa_local(3) = 1
        ELSEIF ((vap3 >= vap1) .AND. (vap3 >= vap2)) THEN
          Sa_local(1) = 1
          Sa_local(2) = 1
        ENDIF
      ELSE
        ! Insert the two highest-voltage SMs for discharging-direction balancing.
        IF ((vap1 <= vap2) .AND. (vap1 <= vap3)) THEN
          Sa_local(2) = 1
          Sa_local(3) = 1
        ELSEIF ((vap2 <= vap1) .AND. (vap2 <= vap3)) THEN
          Sa_local(1) = 1
          Sa_local(3) = 1
        ELSEIF ((vap3 <= vap1) .AND. (vap3 <= vap2)) THEN
          Sa_local(1) = 1
          Sa_local(2) = 1
        ENDIF
      ENDIF

    ELSEIF ($Vap_ref >= (lv1*0.5)) THEN

      IF ($Icma(1) > 0.0) THEN
        ! Insert the lowest-voltage SM for charging-direction balancing.
        IF ((vap1 <= vap2) .AND. (vap1 <= vap3)) THEN
          Sa_local(1) = 1
        ELSEIF ((vap2 <= vap1) .AND. (vap2 <= vap3)) THEN
          Sa_local(2) = 1
        ELSEIF ((vap3 <= vap1) .AND. (vap3 <= vap2)) THEN
          Sa_local(3) = 1
        ENDIF
      ELSE
        ! Insert the highest-voltage SM for discharging-direction balancing.
        IF ((vap1 >= vap2) .AND. (vap1 >= vap3)) THEN
          Sa_local(1) = 1
        ELSEIF ((vap2 >= vap1) .AND. (vap2 >= vap3)) THEN
          Sa_local(2) = 1
        ELSEIF ((vap3 >= vap1) .AND. (vap3 >= vap2)) THEN
          Sa_local(3) = 1
        ENDIF
      ENDIF

    ENDIF

  ENDIF

ENDIF

!------------------ Lower arm A  (SM6 = SiC, SM4/SM5 = Si) ------------------
! Si-SMs perform base NLM. SiC-SM performs PWM residual only when it improves SiC voltage balance.
! If SiC PWM would worsen balancing, PWM is blocked and all three SMs, including SiC, are sorted by NLM.
van1 = $Vcapsa(4)
van2 = $Vcapsa(5)
van3 = $Vcapsa(6)

!------------------ 1) Base NLM insertion using Si-SMs ------------------
base = numan - 1

IF (base < 0) THEN
  base = 0
ENDIF

IF (base > 2) THEN
  base = 2
ENDIF

IF (base == 1) THEN

  IF ($Icma(2) > 0.0) THEN
    IF (van1 <= van2) THEN
      Sa_local(4) = 1
    ELSE
      Sa_local(5) = 1
    ENDIF
  ELSE
    IF (van1 >= van2) THEN
      Sa_local(4) = 1
    ELSE
      Sa_local(5) = 1
    ENDIF
  ENDIF

ELSEIF (base == 2) THEN

  Sa_local(4) = 1
  Sa_local(5) = 1

ENDIF


!------------------ 2) PWM residual decision ------------------
pwm_flag = 0

IF (numan == 1) THEN
  IF ((lv1*$PWM) < $Van_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numan == 2) THEN
  IF (((lv1*$PWM) + lv1) < $Van_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numan == 3) THEN
  IF (((lv1*$PWM) + lv2) < $Van_ref) THEN
    pwm_flag = 1
  ENDIF
ENDIF


!------------------ 3) SiC PWM enable decision ------------------
! VDIFF > 0 : SiC-SM voltage is higher than Si-SM average.
! VDIFF < 0 : SiC-SM voltage is lower than Si-SM average.
VSI_AVG = 0.5 * (van1 + van2)
VDIFF   = van3 - VSI_AVG

sic_en = 0

IF ($Icma(2) > 0.0) THEN
  ! Insertion is treated as charging direction based on the original Si-SM sorting rule.
  IF ((VDIFF < 0.0) .OR. (van3 < VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ELSE
  ! Insertion is treated as discharging direction based on the original Si-SM sorting rule.
  IF ((VDIFF > 0.0) .OR. (van3 > VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ENDIF

! Absolute direction-aware protection for SiC-SM.
IF (van3 < VLOW) THEN
  IF ($Icma(2) < 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF

IF (van3 > VHIGH) THEN
  IF ($Icma(2) > 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF


!------------------ 4) PWM/NLM assignment ------------------
IF (pwm_flag == 1) THEN

  IF (sic_en == 1) THEN

    Sa_local(6) = 1

  ELSE

    ! SiC PWM is disabled. Use NLM sorting including the SiC-SM.
    Sa_local(4) = 0
    Sa_local(5) = 0
    Sa_local(6) = 0

    IF ($Van_ref >= (lv1*2.5)) THEN

      Sa_local(4) = 1
      Sa_local(5) = 1
      Sa_local(6) = 1

    ELSEIF ($Van_ref >= (lv1*1.5)) THEN

      IF ($Icma(2) > 0.0) THEN
        ! Insert the two lowest-voltage SMs for charging-direction balancing.
        IF ((van1 >= van2) .AND. (van1 >= van3)) THEN
          Sa_local(5) = 1
          Sa_local(6) = 1
        ELSEIF ((van2 >= van1) .AND. (van2 >= van3)) THEN
          Sa_local(4) = 1
          Sa_local(6) = 1
        ELSEIF ((van3 >= van1) .AND. (van3 >= van2)) THEN
          Sa_local(4) = 1
          Sa_local(5) = 1
        ENDIF
      ELSE
        ! Insert the two highest-voltage SMs for discharging-direction balancing.
        IF ((van1 <= van2) .AND. (van1 <= van3)) THEN
          Sa_local(5) = 1
          Sa_local(6) = 1
        ELSEIF ((van2 <= van1) .AND. (van2 <= van3)) THEN
          Sa_local(4) = 1
          Sa_local(6) = 1
        ELSEIF ((van3 <= van1) .AND. (van3 <= van2)) THEN
          Sa_local(4) = 1
          Sa_local(5) = 1
        ENDIF
      ENDIF

    ELSEIF ($Van_ref >= (lv1*0.5)) THEN

      IF ($Icma(2) > 0.0) THEN
        ! Insert the lowest-voltage SM for charging-direction balancing.
        IF ((van1 <= van2) .AND. (van1 <= van3)) THEN
          Sa_local(4) = 1
        ELSEIF ((van2 <= van1) .AND. (van2 <= van3)) THEN
          Sa_local(5) = 1
        ELSEIF ((van3 <= van1) .AND. (van3 <= van2)) THEN
          Sa_local(6) = 1
        ENDIF
      ELSE
        ! Insert the highest-voltage SM for discharging-direction balancing.
        IF ((van1 >= van2) .AND. (van1 >= van3)) THEN
          Sa_local(4) = 1
        ELSEIF ((van2 >= van1) .AND. (van2 >= van3)) THEN
          Sa_local(5) = 1
        ELSEIF ((van3 >= van1) .AND. (van3 >= van2)) THEN
          Sa_local(6) = 1
        ENDIF
      ENDIF

    ENDIF

  ENDIF

ENDIF


!====================================================
!                    PHASE B
!====================================================

!------------------ Upper arm B  (SM1 = SiC, SM2/SM3 = Si) ------------------
! Si-SMs perform base NLM. SiC-SM performs PWM residual only when it improves SiC voltage balance.
! If SiC PWM would worsen balancing, PWM is blocked and all three SMs, including SiC, are sorted by NLM.
vap1 = $Vcapsb(1)
vap2 = $Vcapsb(2)
vap3 = $Vcapsb(3)

!------------------ 1) Base NLM insertion using Si-SMs ------------------
base = numbp - 1

IF (base < 0) THEN
  base = 0
ENDIF

IF (base > 2) THEN
  base = 2
ENDIF

IF (base == 1) THEN

  IF ($Icmb(1) > 0.0) THEN
    IF (vap2 <= vap3) THEN
      Sb_local(2) = 1
    ELSE
      Sb_local(3) = 1
    ENDIF
  ELSE
    IF (vap2 >= vap3) THEN
      Sb_local(2) = 1
    ELSE
      Sb_local(3) = 1
    ENDIF
  ENDIF

ELSEIF (base == 2) THEN

  Sb_local(2) = 1
  Sb_local(3) = 1

ENDIF


!------------------ 2) PWM residual decision ------------------
pwm_flag = 0

IF (numbp == 1) THEN
  IF ((lv1*$PWM) < $Vbp_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numbp == 2) THEN
  IF (((lv1*$PWM) + lv1) < $Vbp_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numbp == 3) THEN
  IF (((lv1*$PWM) + lv2) < $Vbp_ref) THEN
    pwm_flag = 1
  ENDIF
ENDIF


!------------------ 3) SiC PWM enable decision ------------------
! VDIFF > 0 : SiC-SM voltage is higher than Si-SM average.
! VDIFF < 0 : SiC-SM voltage is lower than Si-SM average.
VSI_AVG = 0.5 * (vap2 + vap3)
VDIFF   = vap1 - VSI_AVG

sic_en = 0

IF ($Icmb(1) > 0.0) THEN
  ! Insertion is treated as charging direction based on the original Si-SM sorting rule.
  IF ((VDIFF < 0.0) .OR. (vap1 < VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ELSE
  ! Insertion is treated as discharging direction based on the original Si-SM sorting rule.
  IF ((VDIFF > 0.0) .OR. (vap1 > VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ENDIF

! Absolute direction-aware protection for SiC-SM.
IF (vap1 < VLOW) THEN
  IF ($Icmb(1) < 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF

IF (vap1 > VHIGH) THEN
  IF ($Icmb(1) > 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF


!------------------ 4) PWM/NLM assignment ------------------
IF (pwm_flag == 1) THEN

  IF (sic_en == 1) THEN

    Sb_local(1) = 1

  ELSE

    ! SiC PWM is disabled. Use NLM sorting including the SiC-SM.
    Sb_local(1) = 0
    Sb_local(2) = 0
    Sb_local(3) = 0

    IF ($Vbp_ref >= (lv1*2.5)) THEN

      Sb_local(1) = 1
      Sb_local(2) = 1
      Sb_local(3) = 1

    ELSEIF ($Vbp_ref >= (lv1*1.5)) THEN

      IF ($Icmb(1) > 0.0) THEN
        ! Insert the two lowest-voltage SMs for charging-direction balancing.
        IF ((vap1 >= vap2) .AND. (vap1 >= vap3)) THEN
          Sb_local(2) = 1
          Sb_local(3) = 1
        ELSEIF ((vap2 >= vap1) .AND. (vap2 >= vap3)) THEN
          Sb_local(1) = 1
          Sb_local(3) = 1
        ELSEIF ((vap3 >= vap1) .AND. (vap3 >= vap2)) THEN
          Sb_local(1) = 1
          Sb_local(2) = 1
        ENDIF
      ELSE
        ! Insert the two highest-voltage SMs for discharging-direction balancing.
        IF ((vap1 <= vap2) .AND. (vap1 <= vap3)) THEN
          Sb_local(2) = 1
          Sb_local(3) = 1
        ELSEIF ((vap2 <= vap1) .AND. (vap2 <= vap3)) THEN
          Sb_local(1) = 1
          Sb_local(3) = 1
        ELSEIF ((vap3 <= vap1) .AND. (vap3 <= vap2)) THEN
          Sb_local(1) = 1
          Sb_local(2) = 1
        ENDIF
      ENDIF

    ELSEIF ($Vbp_ref >= (lv1*0.5)) THEN

      IF ($Icmb(1) > 0.0) THEN
        ! Insert the lowest-voltage SM for charging-direction balancing.
        IF ((vap1 <= vap2) .AND. (vap1 <= vap3)) THEN
          Sb_local(1) = 1
        ELSEIF ((vap2 <= vap1) .AND. (vap2 <= vap3)) THEN
          Sb_local(2) = 1
        ELSEIF ((vap3 <= vap1) .AND. (vap3 <= vap2)) THEN
          Sb_local(3) = 1
        ENDIF
      ELSE
        ! Insert the highest-voltage SM for discharging-direction balancing.
        IF ((vap1 >= vap2) .AND. (vap1 >= vap3)) THEN
          Sb_local(1) = 1
        ELSEIF ((vap2 >= vap1) .AND. (vap2 >= vap3)) THEN
          Sb_local(2) = 1
        ELSEIF ((vap3 >= vap1) .AND. (vap3 >= vap2)) THEN
          Sb_local(3) = 1
        ENDIF
      ENDIF

    ENDIF

  ENDIF

ENDIF

!------------------ Lower arm B  (SM6 = SiC, SM4/SM5 = Si) ------------------
! Si-SMs perform base NLM. SiC-SM performs PWM residual only when it improves SiC voltage balance.
! If SiC PWM would worsen balancing, PWM is blocked and all three SMs, including SiC, are sorted by NLM.
van1 = $Vcapsb(4)
van2 = $Vcapsb(5)
van3 = $Vcapsb(6)

!------------------ 1) Base NLM insertion using Si-SMs ------------------
base = numbn - 1

IF (base < 0) THEN
  base = 0
ENDIF

IF (base > 2) THEN
  base = 2
ENDIF

IF (base == 1) THEN

  IF ($Icmb(2) > 0.0) THEN
    IF (van1 <= van2) THEN
      Sb_local(4) = 1
    ELSE
      Sb_local(5) = 1
    ENDIF
  ELSE
    IF (van1 >= van2) THEN
      Sb_local(4) = 1
    ELSE
      Sb_local(5) = 1
    ENDIF
  ENDIF

ELSEIF (base == 2) THEN

  Sb_local(4) = 1
  Sb_local(5) = 1

ENDIF


!------------------ 2) PWM residual decision ------------------
pwm_flag = 0

IF (numbn == 1) THEN
  IF ((lv1*$PWM) < $Vbn_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numbn == 2) THEN
  IF (((lv1*$PWM) + lv1) < $Vbn_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numbn == 3) THEN
  IF (((lv1*$PWM) + lv2) < $Vbn_ref) THEN
    pwm_flag = 1
  ENDIF
ENDIF


!------------------ 3) SiC PWM enable decision ------------------
! VDIFF > 0 : SiC-SM voltage is higher than Si-SM average.
! VDIFF < 0 : SiC-SM voltage is lower than Si-SM average.
VSI_AVG = 0.5 * (van1 + van2)
VDIFF   = van3 - VSI_AVG

sic_en = 0

IF ($Icmb(2) > 0.0) THEN
  ! Insertion is treated as charging direction based on the original Si-SM sorting rule.
  IF ((VDIFF < 0.0) .OR. (van3 < VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ELSE
  ! Insertion is treated as discharging direction based on the original Si-SM sorting rule.
  IF ((VDIFF > 0.0) .OR. (van3 > VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ENDIF

! Absolute direction-aware protection for SiC-SM.
IF (van3 < VLOW) THEN
  IF ($Icmb(2) < 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF

IF (van3 > VHIGH) THEN
  IF ($Icmb(2) > 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF


!------------------ 4) PWM/NLM assignment ------------------
IF (pwm_flag == 1) THEN

  IF (sic_en == 1) THEN

    Sb_local(6) = 1

  ELSE

    ! SiC PWM is disabled. Use NLM sorting including the SiC-SM.
    Sb_local(4) = 0
    Sb_local(5) = 0
    Sb_local(6) = 0

    IF ($Vbn_ref >= (lv1*2.5)) THEN

      Sb_local(4) = 1
      Sb_local(5) = 1
      Sb_local(6) = 1

    ELSEIF ($Vbn_ref >= (lv1*1.5)) THEN

      IF ($Icmb(2) > 0.0) THEN
        ! Insert the two lowest-voltage SMs for charging-direction balancing.
        IF ((van1 >= van2) .AND. (van1 >= van3)) THEN
          Sb_local(5) = 1
          Sb_local(6) = 1
        ELSEIF ((van2 >= van1) .AND. (van2 >= van3)) THEN
          Sb_local(4) = 1
          Sb_local(6) = 1
        ELSEIF ((van3 >= van1) .AND. (van3 >= van2)) THEN
          Sb_local(4) = 1
          Sb_local(5) = 1
        ENDIF
      ELSE
        ! Insert the two highest-voltage SMs for discharging-direction balancing.
        IF ((van1 <= van2) .AND. (van1 <= van3)) THEN
          Sb_local(5) = 1
          Sb_local(6) = 1
        ELSEIF ((van2 <= van1) .AND. (van2 <= van3)) THEN
          Sb_local(4) = 1
          Sb_local(6) = 1
        ELSEIF ((van3 <= van1) .AND. (van3 <= van2)) THEN
          Sb_local(4) = 1
          Sb_local(5) = 1
        ENDIF
      ENDIF

    ELSEIF ($Vbn_ref >= (lv1*0.5)) THEN

      IF ($Icmb(2) > 0.0) THEN
        ! Insert the lowest-voltage SM for charging-direction balancing.
        IF ((van1 <= van2) .AND. (van1 <= van3)) THEN
          Sb_local(4) = 1
        ELSEIF ((van2 <= van1) .AND. (van2 <= van3)) THEN
          Sb_local(5) = 1
        ELSEIF ((van3 <= van1) .AND. (van3 <= van2)) THEN
          Sb_local(6) = 1
        ENDIF
      ELSE
        ! Insert the highest-voltage SM for discharging-direction balancing.
        IF ((van1 >= van2) .AND. (van1 >= van3)) THEN
          Sb_local(4) = 1
        ELSEIF ((van2 >= van1) .AND. (van2 >= van3)) THEN
          Sb_local(5) = 1
        ELSEIF ((van3 >= van1) .AND. (van3 >= van2)) THEN
          Sb_local(6) = 1
        ENDIF
      ENDIF

    ENDIF

  ENDIF

ENDIF


!====================================================
!                    PHASE C
!====================================================

!------------------ Upper arm C  (SM1 = SiC, SM2/SM3 = Si) ------------------
! Si-SMs perform base NLM. SiC-SM performs PWM residual only when it improves SiC voltage balance.
! If SiC PWM would worsen balancing, PWM is blocked and all three SMs, including SiC, are sorted by NLM.
vap1 = $Vcapsc(1)
vap2 = $Vcapsc(2)
vap3 = $Vcapsc(3)

!------------------ 1) Base NLM insertion using Si-SMs ------------------
base = numcp - 1

IF (base < 0) THEN
  base = 0
ENDIF

IF (base > 2) THEN
  base = 2
ENDIF

IF (base == 1) THEN

  IF ($Icmc(1) > 0.0) THEN
    IF (vap2 <= vap3) THEN
      Sc_local(2) = 1
    ELSE
      Sc_local(3) = 1
    ENDIF
  ELSE
    IF (vap2 >= vap3) THEN
      Sc_local(2) = 1
    ELSE
      Sc_local(3) = 1
    ENDIF
  ENDIF

ELSEIF (base == 2) THEN

  Sc_local(2) = 1
  Sc_local(3) = 1

ENDIF


!------------------ 2) PWM residual decision ------------------
pwm_flag = 0

IF (numcp == 1) THEN
  IF ((lv1*$PWM) < $Vcp_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numcp == 2) THEN
  IF (((lv1*$PWM) + lv1) < $Vcp_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numcp == 3) THEN
  IF (((lv1*$PWM) + lv2) < $Vcp_ref) THEN
    pwm_flag = 1
  ENDIF
ENDIF


!------------------ 3) SiC PWM enable decision ------------------
! VDIFF > 0 : SiC-SM voltage is higher than Si-SM average.
! VDIFF < 0 : SiC-SM voltage is lower than Si-SM average.
VSI_AVG = 0.5 * (vap2 + vap3)
VDIFF   = vap1 - VSI_AVG

sic_en = 0

IF ($Icmc(1) > 0.0) THEN
  ! Insertion is treated as charging direction based on the original Si-SM sorting rule.
  IF ((VDIFF < 0.0) .OR. (vap1 < VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ELSE
  ! Insertion is treated as discharging direction based on the original Si-SM sorting rule.
  IF ((VDIFF > 0.0) .OR. (vap1 > VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ENDIF

! Absolute direction-aware protection for SiC-SM.
IF (vap1 < VLOW) THEN
  IF ($Icmc(1) < 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF

IF (vap1 > VHIGH) THEN
  IF ($Icmc(1) > 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF


!------------------ 4) PWM/NLM assignment ------------------
IF (pwm_flag == 1) THEN

  IF (sic_en == 1) THEN

    Sc_local(1) = 1

  ELSE

    ! SiC PWM is disabled. Use NLM sorting including the SiC-SM.
    Sc_local(1) = 0
    Sc_local(2) = 0
    Sc_local(3) = 0

    IF ($Vcp_ref >= (lv1*2.5)) THEN

      Sc_local(1) = 1
      Sc_local(2) = 1
      Sc_local(3) = 1

    ELSEIF ($Vcp_ref >= (lv1*1.5)) THEN

      IF ($Icmc(1) > 0.0) THEN
        ! Insert the two lowest-voltage SMs for charging-direction balancing.
        IF ((vap1 >= vap2) .AND. (vap1 >= vap3)) THEN
          Sc_local(2) = 1
          Sc_local(3) = 1
        ELSEIF ((vap2 >= vap1) .AND. (vap2 >= vap3)) THEN
          Sc_local(1) = 1
          Sc_local(3) = 1
        ELSEIF ((vap3 >= vap1) .AND. (vap3 >= vap2)) THEN
          Sc_local(1) = 1
          Sc_local(2) = 1
        ENDIF
      ELSE
        ! Insert the two highest-voltage SMs for discharging-direction balancing.
        IF ((vap1 <= vap2) .AND. (vap1 <= vap3)) THEN
          Sc_local(2) = 1
          Sc_local(3) = 1
        ELSEIF ((vap2 <= vap1) .AND. (vap2 <= vap3)) THEN
          Sc_local(1) = 1
          Sc_local(3) = 1
        ELSEIF ((vap3 <= vap1) .AND. (vap3 <= vap2)) THEN
          Sc_local(1) = 1
          Sc_local(2) = 1
        ENDIF
      ENDIF

    ELSEIF ($Vcp_ref >= (lv1*0.5)) THEN

      IF ($Icmc(1) > 0.0) THEN
        ! Insert the lowest-voltage SM for charging-direction balancing.
        IF ((vap1 <= vap2) .AND. (vap1 <= vap3)) THEN
          Sc_local(1) = 1
        ELSEIF ((vap2 <= vap1) .AND. (vap2 <= vap3)) THEN
          Sc_local(2) = 1
        ELSEIF ((vap3 <= vap1) .AND. (vap3 <= vap2)) THEN
          Sc_local(3) = 1
        ENDIF
      ELSE
        ! Insert the highest-voltage SM for discharging-direction balancing.
        IF ((vap1 >= vap2) .AND. (vap1 >= vap3)) THEN
          Sc_local(1) = 1
        ELSEIF ((vap2 >= vap1) .AND. (vap2 >= vap3)) THEN
          Sc_local(2) = 1
        ELSEIF ((vap3 >= vap1) .AND. (vap3 >= vap2)) THEN
          Sc_local(3) = 1
        ENDIF
      ENDIF

    ENDIF

  ENDIF

ENDIF

!------------------ Lower arm C  (SM6 = SiC, SM4/SM5 = Si) ------------------
! Si-SMs perform base NLM. SiC-SM performs PWM residual only when it improves SiC voltage balance.
! If SiC PWM would worsen balancing, PWM is blocked and all three SMs, including SiC, are sorted by NLM.
van1 = $Vcapsc(4)
van2 = $Vcapsc(5)
van3 = $Vcapsc(6)

!------------------ 1) Base NLM insertion using Si-SMs ------------------
base = numcn - 1

IF (base < 0) THEN
  base = 0
ENDIF

IF (base > 2) THEN
  base = 2
ENDIF

IF (base == 1) THEN

  IF ($Icmc(2) > 0.0) THEN
    IF (van1 <= van2) THEN
      Sc_local(4) = 1
    ELSE
      Sc_local(5) = 1
    ENDIF
  ELSE
    IF (van1 >= van2) THEN
      Sc_local(4) = 1
    ELSE
      Sc_local(5) = 1
    ENDIF
  ENDIF

ELSEIF (base == 2) THEN

  Sc_local(4) = 1
  Sc_local(5) = 1

ENDIF


!------------------ 2) PWM residual decision ------------------
pwm_flag = 0

IF (numcn == 1) THEN
  IF ((lv1*$PWM) < $Vcn_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numcn == 2) THEN
  IF (((lv1*$PWM) + lv1) < $Vcn_ref) THEN
    pwm_flag = 1
  ENDIF
ELSEIF (numcn == 3) THEN
  IF (((lv1*$PWM) + lv2) < $Vcn_ref) THEN
    pwm_flag = 1
  ENDIF
ENDIF


!------------------ 3) SiC PWM enable decision ------------------
! VDIFF > 0 : SiC-SM voltage is higher than Si-SM average.
! VDIFF < 0 : SiC-SM voltage is lower than Si-SM average.
VSI_AVG = 0.5 * (van1 + van2)
VDIFF   = van3 - VSI_AVG

sic_en = 0

IF ($Icmc(2) > 0.0) THEN
  ! Insertion is treated as charging direction based on the original Si-SM sorting rule.
  IF ((VDIFF < 0.0) .OR. (van3 < VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ELSE
  ! Insertion is treated as discharging direction based on the original Si-SM sorting rule.
  IF ((VDIFF > 0.0) .OR. (van3 > VSM_REF)) THEN
    sic_en = 1
  ELSE
    sic_en = 0
  ENDIF
ENDIF

! Absolute direction-aware protection for SiC-SM.
IF (van3 < VLOW) THEN
  IF ($Icmc(2) < 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF

IF (van3 > VHIGH) THEN
  IF ($Icmc(2) > 0.0) THEN
    sic_en = 0
  ELSE
    sic_en = 1
  ENDIF
ENDIF


!------------------ 4) PWM/NLM assignment ------------------
IF (pwm_flag == 1) THEN

  IF (sic_en == 1) THEN

    Sc_local(6) = 1

  ELSE

    ! SiC PWM is disabled. Use NLM sorting including the SiC-SM.
    Sc_local(4) = 0
    Sc_local(5) = 0
    Sc_local(6) = 0

    IF ($Vcn_ref >= (lv1*2.5)) THEN

      Sc_local(4) = 1
      Sc_local(5) = 1
      Sc_local(6) = 1

    ELSEIF ($Vcn_ref >= (lv1*1.5)) THEN

      IF ($Icmc(2) > 0.0) THEN
        ! Insert the two lowest-voltage SMs for charging-direction balancing.
        IF ((van1 >= van2) .AND. (van1 >= van3)) THEN
          Sc_local(5) = 1
          Sc_local(6) = 1
        ELSEIF ((van2 >= van1) .AND. (van2 >= van3)) THEN
          Sc_local(4) = 1
          Sc_local(6) = 1
        ELSEIF ((van3 >= van1) .AND. (van3 >= van2)) THEN
          Sc_local(4) = 1
          Sc_local(5) = 1
        ENDIF
      ELSE
        ! Insert the two highest-voltage SMs for discharging-direction balancing.
        IF ((van1 <= van2) .AND. (van1 <= van3)) THEN
          Sc_local(5) = 1
          Sc_local(6) = 1
        ELSEIF ((van2 <= van1) .AND. (van2 <= van3)) THEN
          Sc_local(4) = 1
          Sc_local(6) = 1
        ELSEIF ((van3 <= van1) .AND. (van3 <= van2)) THEN
          Sc_local(4) = 1
          Sc_local(5) = 1
        ENDIF
      ENDIF

    ELSEIF ($Vcn_ref >= (lv1*0.5)) THEN

      IF ($Icmc(2) > 0.0) THEN
        ! Insert the lowest-voltage SM for charging-direction balancing.
        IF ((van1 <= van2) .AND. (van1 <= van3)) THEN
          Sc_local(4) = 1
        ELSEIF ((van2 <= van1) .AND. (van2 <= van3)) THEN
          Sc_local(5) = 1
        ELSEIF ((van3 <= van1) .AND. (van3 <= van2)) THEN
          Sc_local(6) = 1
        ENDIF
      ELSE
        ! Insert the highest-voltage SM for discharging-direction balancing.
        IF ((van1 >= van2) .AND. (van1 >= van3)) THEN
          Sc_local(4) = 1
        ELSEIF ((van2 >= van1) .AND. (van2 >= van3)) THEN
          Sc_local(5) = 1
        ELSEIF ((van3 >= van1) .AND. (van3 >= van2)) THEN
          Sc_local(6) = 1
        ENDIF
      ENDIF

    ENDIF

  ENDIF

ENDIF


!------------------ Output ------------------

$Sa = Sa_local
$Sb = Sb_local
$Sc = Sc_local
