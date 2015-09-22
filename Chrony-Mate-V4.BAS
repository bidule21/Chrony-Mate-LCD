' Program chrony.bas
' ***********************************************************************
' Chrony External Display
' ***********************************************************************
'Compatibility with 16F628
@    device  pic16F628, INTRC_OSC_NOCLKOUT, wdt_on, pwrt_on, mclr_off, bod_on, lvp_off   ;Config switches, different from PBP default


    Include "Modedefs.bas"

        
	DEFINE OSC		   4		' Set the Xtal frequency to 4mHz

' ** Declare LCDOUT Defines **

	DEFINE	LCD_DREG	PortB			' Set Data to PortB
	DEFINE 	LCD_DBIT	0			' Set starting Data to Bit0
	DEFINE 	LCD_RSREG  	PortA			' Set Register Select to PortA
	DEFINE 	LCD_RSBIT   2			' Set RS line to PORTA.2
	DEFINE 	LCD_EREG	PortA			' Set Enable to PortA
	DEFINE 	LCD_EBIT  	3			' Set Enable line to PortA.3
	DEFINE 	LCD_BITS	8			' Set for 8 bit Bus
	DEFINE 	LCD_LINES	4			' Set number of lines to 2 (my LCD works like 4 but really has 2)
'	DEFINE  LCD_COMMANDUS 1000	'Set command delay time in us
'	DEFINE  LCD_DATAUS 50		'Set data delay time in us
 
' ** Define LCD Control Constants **

	LcdCom	Con	254			' LCD Control Byte, preceeds an LCD command.
	Clr		Con	1			' Clear the display, LCD Command
	Quad1	Con	128			'Beginning of line 1, Quadrant 1, LCD Command
	Quad2	Con	136			'Line 1, Quadrant 2, LCD Command
    Quad3	Con	192			'Beginning of line 2, Quadrant 3, LCD Command
	Quad4	Con	200			'Line 2, Quadrant 4, LCD Command
	Cgram	Con	64			' Point to Cgram within LCD, LCD Command
'	Shift_L	Con	24			' Shift display left, LCD Command
'	Shift_R	Con	28			' Shift display right, LCD Command

' ** Declare Variables **

	SI		Var	PortA.0			' Serial In 
	LED		Var	PortA.1			' LED shot good indicator
    Reset_all 	Var	PortB.4		' Pin for clearing shots data
	Text_in var Byte[8]         'Eight byte array for serial input
    fps var word
    RAW var Word
    Work_var1 var word
    Accum var word
    Ave_fps var word
    Max_fps var Word
    Min_fps var WORD
'   Work_var3 var Word    
    Rcvbyte	Var	Byte				' The byte received 
    Rcvbyte2 Var byte
    Shot_num var byte
    Work_var var byte
'	Var	Byte		' The bargraph position byte received

    'PIC16F628 compatibilty
    VRCON.7 = %0    '
    VRCON.6 = %0    'Comparator voltage reference OFF
    CMCON = %111    'Comparator inputs to OFF  

Init:
    Pause 150					' Wait for LCD to Initialize 
    fps = 0
    RAW = 0
    Work_var1 = 0
    Rcvbyte = 0				' The byte received 
    Rcvbyte2 = 0
    Shot_num = 0
    Work_var = 0
    Accum = 0
    Ave_fps = 0
    Max_fps = 0
    Min_fps = $ffff
   	Lcdout LcdCom,Clr:Pause 30			' Clear the LCD	
	Gosub Clr_It					' Initialize the LCD	
	Lcdout LcdCom,Quad1,"Chrony Mate V1.0"		' Print message			
	Lcdout LcdCom,Quad3,"Waiting.....    "		' Print message			

'CODE LOOP STARTS HERE
'------------------------------    
Start:
    Lcdout LcdCom,Quad1:pause 30
    
    Serin2 SI,188, [wait("-"), skip 3, rcvbyte]   'look for a "-" dash incomming, skip three chars then save a char.

    if Rcvbyte = "," then
         Serin2 SI,188,[wait(" "), STR Text_in\7] 'If saved char is a "," then wait for a space then store next 7 chars.
         rcvbyte2 = $30
         Goto Good_shot
        Endif
        
    if Rcvbyte = "E" then
            Serin2 SI,188,[skip 3, rcvbyte2]
            LCDout LcdCom,Quad2,"Error ",Rcvbyte2," "
            Goto Start 
        Else
            Gosub LCD_CLR
            LCDOUT " COMM's ERROR 1" 
            Goto Start     
        endif

Good_shot:
        
    If Text_in[0] = $20 then     'Is stored charactor null (a space).
            
            Else 
                gosub LCD_CLR
                LCDout LcdCom,Quad2,"Too SLOW"
                Goto Start
            Endif 

    If Text_in[1] = $20 then
            
            Else 
                gosub LCD_CLR
                LCDout LcdCom,Quad2,"Too Slow"
                Goto Start
            Endif
             
   If Text_in[2] < $33 then
                    
            Else 
                gosub LCD_CLR
                LCDout LcdCom,Quad2,"Too slow"
                Goto Start
            Endif  
                 
'---------- Check for non-numerals in the shot data------  
  for Work_var = 2 to 6   
    If $30 > Text_in[work_var] > $39 then
        If Text_in[work_var] != $20 then    'spaces are OK   
            Gosub LCD_CLR
            LCDOUT " COMM's ERROR 2"
            goto start
        Endif
    Endif 
  Next Work_var  
  
'------Convert 7 bytes of text numerals to 1 byte value            
    Raw = 0 
    If  Text_in[2] = $30 or Text_in[2] = $20 then        'don't multiply zero!
        Else         
            Raw = (Text_in[2] - $30) * 10000
        Endif
    If Text_in[3] = $30 or Text_in[3] = $20 then
        Else
            RAW = ((Text_in[3] - $30) * 1000) + RAW
        Endif
    If Text_in[4] = $30 or Text_in[4] = $20 then
        Else
            RAw = ((Text_in[4] - $30) * 100) + RAW 
        Endif
    If Text_in[5] = $30 then
        Else
            Raw = ((Text_in[5] - $30) * 10) + RAW
        Endif
    If Text_in[6] = $30 then
        Else
            Raw = (Text_in[6] - $30) + RAW
        Endif
'--------Shot counter------------
    shot_num = shot_num + 1         'increment shot counter
    
'-------------------------------------------------------------------------
Calc32:
    R0.HIGHBYTE = $00		'stuff internal registers with 00B71B00hex=12000000dec
    R0.LOWBYTE = $B7
    R2.HIGHBYTE = $1B
    R2.LOWBYTE = $00 

    fps = DIV32 Raw	'do the division, make sure Raw is <= 15 bits (32767)

    Accum = Accum + fps
    ave_fps = Accum / shot_num
'    Work_var1 = Accum // shot_num 'ave not work as divisor is variable
    
'    If  work_var1 >= 5 then
'        ave_fps = ave_fps + 1
'    Endif
        
    If fps > Max_fps then
        Max_fps = fps
    Endif
    
    If fps < Min_fps then
        Min_fps = fps
    Endif
     
    Work_var1 = Max_fps - Min_fps
    
    Gosub LCD_CLR 
	Lcdout "Shot ",#shot_num
	Lcdout LcdCom,Quad2,"fps ", #fps
'    If fps_dec > 49 then fps = fps + 1  'round up to nearest UNIT	
	LCDOUT LCDCom,Quad4, "Ave ", #Ave_fps
'	LCDOUT LCDCom,Quad2, STR Text_in\7  'number in text array
    lcdout LCDCom,quad3, "Var ", #Work_var1          'number in Raw
    
    LED High    'Bright green LED on
    Pause 5000  'for 5 seconds
    LED LOW     'LED Off

	Goto Start				' Loop back forever

'--------------------------------------------------------
LCD_CLR:
       	Lcdout LcdCom,Clr:Pause 30			' Clear the LCD	    
       	Return

 
' Initialize LCD ready to print
Clr_It:
	TrisB=0					' Set Port to Output
	Pause 	100				' Wait for LCD to Initialise
	Lcdout LcdCom,Clr:Pause 30			' Clear the LCD	
	Lcdout LcdCom,Quad1," ANY TEXT HERE  "      'initalizes LCD macro in PBP
	Return
