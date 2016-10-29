C Run counters (started by AO)
C MT adds counters for the number of recoils that make it
C   to Q3 (Sext 1) and to Q8 (Quad 6).
C MT adds counter for the number of beam particles that reach
C   the end detector.

	Integer*4 ngascell,nreact,ntargexit,nbeamout,nend,nfcm2,
     &            Num_Recoils_Q3, Num_Recoils_Q8,
     &            Num_BeamPart_ENDV
	Common/cntrs/ngascell,nreact,ntargexit,nbeamout,nend,nfcm2,
     &               Num_Recoils_Q3, Num_Recoils_Q8,
     &               Num_BeamPart_ENDV
