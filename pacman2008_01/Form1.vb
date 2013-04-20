Public Class Form1
    'basics:  each icon is drawn to be 32 by 32 pixels, and the entire maze is based on this
    '(this is despite the fact the icons originally start 33x33).  The reason is 32 is much easier
    'to divide.  Movement of pacman and the badguys is based on a half or a fourth of 
    'the size of the icons.

    Inherits System.Windows.Forms.Form
    Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Integer) As Short
    'ground is 25 blocks wide
    'Ground is 6 blocks high
    'screen must be 693 high
    'last available picture is at 302
    'control array pictures available after index 154
    'Dim x, y As Integer
    'Const VK_A = &H41  'A
    'Const VK_Z = &H54  'Z
    'Const VK_LSHIFT = &HA0
    'Const VK_RSHIFT = &HA1
    'Const VK_LKCONTROL = &HA2
    'Const VK_RKCONTROL = &HA3
    'Const VK_RIGHT = &H27
    'Const VK_LEFT = &H25
    'Const VK_UP = &H26
    'Const VK_DOWN = &H28
    'Const VK_SPACE = &H20
    Const VK_ESC = &H1B
    Dim blockxPosition As Integer = 0
    Dim direction As String
    Dim keypressed As String
    'Dim pict(315) As PictureBox
    Private bigdots() As PictureBox = {} 'dots
    Private pict() As PictureBox = {}    'maze blocks

    Private Sub btnNewTextBox_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    ' The user entered some text.
    Private Sub TextBox_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        ' Display the current text.
        Dim txt As TextBox = DirectCast(sender, TextBox)
        Debug.WriteLine(txt.Name & ": [" & txt.Text & "]")
    End Sub

    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        WhichKey = e.KeyCode 'KeyCode
        'Label1.Text = WhichKey
        TurningSub()
        If ShouldExit = True Then
            ShouldExit = False
            Exit Sub
        End If
        If e.KeyCode = 39 Then 'right
            If turning = True And PacMan.Top Mod 32 <> 0 Then Exit Sub
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Timer1.Enabled = True
        End If
        If e.KeyCode = 37 Then 'left
            If turning = True And PacMan.Top Mod 32 <> 0 Then Exit Sub
            Timer1.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Timer2.Enabled = True
        End If
        If e.KeyCode = 40 Then  'down
            If turning = True And PacMan.Left Mod 32 <> 0 Then Exit Sub
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer4.Enabled = False
            Timer3.Enabled = True
        End If
        'If KeyCode = vbKeyUp Then
        If e.KeyCode = 38 Then 'up
            If turning = True And PacMan.Left Mod 32 <> 0 Then Exit Sub
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = True
        End If
        'If KeyCode = vbKeyEscape Then End
        If e.KeyCode = VK_ESC Then End
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim i As Integer
        For i = 0 To 317
            i = pict.Length
            i = bigdots.Length
            ReDim Preserve pict(i)
            ReDim Preserve bigdots(i)
            pict(i) = New PictureBox
            bigdots(i) = New PictureBox
            pict(i).SizeMode = PictureBoxSizeMode.AutoSize
            bigdots(i).SizeMode = PictureBoxSizeMode.AutoSize
            pict(i).Image = PictureBox1.Image
            bigdots(i).Image = dot03.Image
            pict(i).Visible = False
            bigdots(i).Visible = False
            pict(i).Name = "TextBox" & i.ToString()
            bigdots(i).Name = "TextBox" & i.ToString()
            If pict.Length > 1 Then
                pict(i).Left = pict(i - 1).Left
                bigdots(i).Left = bigdots(i - 1).Left
                pict(i).Top = pict(i - 1).Top + pict(i - 1).Height + 4
                bigdots(i).Top = bigdots(i - 1).Top + bigdots(i - 1).Height + 4
                pict(i).Size = pict(i - 1).Size
                bigdots(i).Size = bigdots(i - 1).Size
            End If
            pict(i).Tag = i
            bigdots(i).Tag = i
            AddHandler pict(i).TextChanged, AddressOf TextBox_TextChanged
            AddHandler bigdots(i).TextChanged, AddressOf TextBox_TextChanged
            Me.Controls.Add(pict(i))
            Me.Controls.Add(bigdots(i))
        Next i
        reset()
        loadMaze()


    End Sub
    Sub loadMaze()
        turning = False
        PacManRow = 2
        PacManColumn = 2
        Dim a(13) As String
        Dim count As Integer
        Dim x As Integer
        Dim i, j, y, rows, columns, movedown, moveacross As Integer
        rows = 14
        columns = 20
        first = first + 1
        If first > 3 Then first = 3
        'first = 3
        If first = 1 Then
            a(1) = "1111111111111111111"   'the first maze which is loaded
            a(2) = "1222222222212222231"
            a(3) = "1211121111222111121"
            a(4) = "1211121111112111121"
            a(5) = "1211121000012111121"
            a(6) = "1222221000012111121"
            a(7) = "1211121000012111121"
            a(8) = "1211121111112222221"
            a(9) = "1211122222222111121"
            a(10) = "1222112111112111121"
            a(11) = "1212112111112111121"
            a(12) = "1322222222222222231"
            a(13) = "1111111111111111111"
        End If
        If first = 2 Then
            a(1) = "1111111111111111111"
            a(2) = "1222221111112222231"
            a(3) = "1211122222222111121"
            a(4) = "1211121111112111221"
            a(5) = "1211121000012111211"
            a(6) = "1222221000012222211"
            a(7) = "1211121000012111211"
            a(8) = "1222221111112222221"
            a(9) = "1211122222222111121"
            a(10) = "1211112111112111121"
            a(11) = "1211112111112111121"
            a(12) = "1322222222222222231"
            a(13) = "1111111111111111111"
        End If
        If first = 3 Then
            a(1) = "1111111111111111111"
            a(2) = "1222222122212222231"
            a(3) = "1211212221222111121"
            a(4) = "1221221111112121221"
            a(5) = "1121121000012121211"
            a(6) = "1222221000012222221"
            a(7) = "1121121000012111211"
            a(8) = "1222221111112222221"
            a(9) = "1211122222222111121"
            a(10) = "1212212121212122221"
            a(11) = "1212112121212121121"
            a(12) = "1322222221222222231"
            a(13) = "1111111111111111111"
        End If

        For count = 1 To 20
            For x = 1 To 13
                maze(x, count) = Mid(a(x), count, 1)
            Next x
        Next count

        movedown = pict(0).Height
        moveacross = pict(0).Width
        pict(0).Visible = False
        bigdots(0).Visible = 0
        For i = 0 To rows
            For j = 0 To columns
                y = y + 1
                If first > 1 Then pict(y).Visible = 0 'necessary to remove blocks from previous maze which would still be seen
                If maze(i, j) = "1" Then                  'gives a cool effect too of blocks being eliminated
                    If first = 1 Then pict(y).Image = Wall.Image
                    pict(y).Top = movedown * (i - 1)
                    pict(y).Left = movedown * (j - 1)
                    pict(y).Visible = 1
                End If  'next line allows later mazes to locate blocks anywhere
                If maze(i, j) <> "1" And first = 1 Then pict(y).Image = Wall.Image
                If maze(i, j) = "2" Then
                    If first = 1 Then
                        bigdots(y).Visible = 1
                    End If
                    bigdots(y).Top = movedown * (i - 1)
                    bigdots(y).Left = movedown * (j - 1)
                    bigdots(y).Visible = 1
                    bead(i, j) = y
                End If
                If maze(i, j) <> "2" And maze(i, j) <> "3" And first = 1 Then bigdots(i).Image = dot05.Image
                If maze(i, j) = "3" Then
                    If first = 1 Then bigdots(i).Image = dot05.Image
                    bigdots(y).Image = dot05.Image
                    bigdots(y).Top = movedown * (i - 1)
                    bigdots(y).Left = movedown * (j - 1)
                    bigdots(y).Visible = 1
                    bead(i, j) = y
                End If
            Next j
        Next i
        AxWindowsMediaPlayer1.URL = "Maryhay.mid"
    End Sub

    Sub BADGUYS1CONTROL()  'stuck here  badguy gets stopped needs to first see which ways he can turn, then choose if possible a direction that takes pacman toward pacman
        Dim direct As Integer
        y1 = PacMan.Top - BadMan1.Top   'sees how far up or down form pacman to badguy1
        x1 = PacMan.Left - BadMan1.Left  'sees how far left or right  badguy1 is from pacman
        '*******************dead end handler
        If maze(BADman1ROW + 1, BADman1COLUMN) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" And maze(BADman1ROW, BADman1COLUMN + 1) = "1" Then 'dead end above
            Timer8.Enabled = False  'dead end to right go left
            Timer7.Enabled = False
            Timer5.Enabled = True
            Timer6.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW + 1, BADman1COLUMN) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" And maze(BADman1ROW, BADman1COLUMN - 1) = "1" Then 'dead end above
            Timer8.Enabled = False  'dead end to left go right
            Timer7.Enabled = False
            Timer5.Enabled = False
            Timer6.Enabled = True
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW + 1, BADman1COLUMN) = "1" Then 'dead end above
            Timer8.Enabled = False  'dead end below go up
            Timer7.Enabled = True
            Timer5.Enabled = False
            Timer6.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" Then 'dead end above
            Timer8.Enabled = True  'dead end above go down
            Timer7.Enabled = False
            Timer5.Enabled = False
            Timer6.Enabled = False
            Exit Sub
        End If
        '****************************************** elbow handler
        If maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer8.Enabled = True  'elbow to upper left, can go down or right, go down
                Timer6.Enabled = False
            End If
            If direct = 2 Then     'elbow to upper left, can go down or right, go right
                Timer6.Enabled = True
                Timer8.Enabled = False
            End If
            Timer7.Enabled = False
            Timer5.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer5.Enabled = True  'elbow to upper right, can go down or left so go left
                Timer8.Enabled = False
            End If
            If direct = 2 Then
                Timer5.Enabled = False 'elbow to upper right, can go down or left so go left
                Timer8.Enabled = True
            End If
            Timer7.Enabled = False
            Timer6.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW + 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer6.Enabled = True
                Timer7.Enabled = False
            End If
            If direct = 2 Then
                Timer7.Enabled = True
                Timer6.Enabled = False
            End If
            Timer8.Enabled = False  'elbow to lower left, can go up or right so go right
            Timer5.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW + 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1   '!!!!!!!!!!!!went in here before getting lost
            If direct = 1 Then
                Timer5.Enabled = True
                Timer7.Enabled = False
            End If
            If direct = 2 Then
                Timer7.Enabled = True
                Timer5.Enabled = False
            End If
            Timer8.Enabled = False  'elbow to lower right, can go up or left so go left
            Timer6.Enabled = False
            Exit Sub
        End If
        '****************************************** end of the road handler
        If eatbad = 0 Then   'CHASE PACMAN
            If BadMan1.Left Mod 32 = 0 And y1 < 0 Then
                If maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then
                    Timer8.Enabled = False  'open above, pacman above pacman goes up
                    Timer7.Enabled = True
                    Timer5.Enabled = False
                    Timer6.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan1.Left Mod 32 = 0 And y1 > 0 And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                Timer7.Enabled = False   'open below, pacman below badguy goes down
                Timer8.Enabled = True
                Timer5.Enabled = False
                Timer6.Enabled = False
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 > 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                Timer7.Enabled = False   'open right pacman right badguy goes right
                Timer8.Enabled = False
                Timer5.Enabled = False
                Timer6.Enabled = True
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 < 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                Timer7.Enabled = False   'open right pacman LEFT badguy goes left
                Timer8.Enabled = False
                Timer5.Enabled = True
                Timer6.Enabled = False
                Exit Sub
            End If
        End If
        '******************************************************
        If eatbad = 1 Then   'CHASE PACMAN
            If BadMan1.Left Mod 32 = 0 And y1 < 0 Then
                If maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                    Timer8.Enabled = True  'open BELOW, pacman above  go DOWN
                    Timer7.Enabled = False
                    Timer5.Enabled = False
                    Timer6.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan1.Left Mod 32 = 0 And y1 > 0 And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then
                Timer7.Enabled = True   'open ABOVE, pacman below  go UP
                Timer8.Enabled = False
                Timer5.Enabled = False
                Timer6.Enabled = False
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 > 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                Timer7.Enabled = False   'open LEFT pacman right badguy goes LEFT
                Timer8.Enabled = False
                Timer5.Enabled = True
                Timer6.Enabled = False
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 < 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                Timer7.Enabled = False   'open RIGHT pacman LEFT badguy goes RIGHT
                Timer8.Enabled = False
                Timer5.Enabled = False
                Timer6.Enabled = True
                Exit Sub
            End If
        End If
    End Sub
    Sub badguys2control()
        Dim direct As Integer
        y2 = PacMan.Top - BadMan2.Top   'sees how far up or down form pacman to badguy2
        x2 = PacMan.Left - BadMan2.Left  'sees how far left or right  badguy2 is from pacman
        '*******************dead end handler
        If maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" Then 'dead end above
            Timer10.Enabled = False  'dead end to right go left
            Timer11.Enabled = False
            Timer9.Enabled = True
            Timer12.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" Then 'dead end above
            Timer9.Enabled = False  'dead end to left go right
            Timer11.Enabled = False
            Timer12.Enabled = False
            Timer10.Enabled = True
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" Then 'dead end above
            Timer9.Enabled = False  'dead end below go up
            Timer11.Enabled = True
            Timer10.Enabled = False
            Timer12.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" Then 'dead end above
            Timer12.Enabled = True  'dead end above go down
            Timer9.Enabled = False
            Timer10.Enabled = False
            Timer11.Enabled = False
            Exit Sub
        End If
        '****************************************** elbow handler
        If maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer12.Enabled = True  'elbow to upper left, can go down or right, go down
                Timer10.Enabled = False
            End If
            If direct = 2 Then     'elbow to upper left, can go down or right, go right
                Timer10.Enabled = True
                Timer12.Enabled = False
            End If
            Timer9.Enabled = False
            Timer11.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer9.Enabled = True  'elbow to upper right, can go down or left so go left
                Timer12.Enabled = False
            End If
            If direct = 2 Then
                Timer9.Enabled = False 'elbow to upper right, can go down or left so go left
                Timer12.Enabled = True
            End If
            Timer10.Enabled = False
            Timer11.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer10.Enabled = True
                Timer11.Enabled = False
            End If
            If direct = 2 Then
                Timer11.Enabled = True
                Timer10.Enabled = False
            End If
            Timer12.Enabled = False  'elbow to lower left, can go up or right so go right
            Timer9.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1   '!!!!!!!!!!!!went in here before getting lost
            If direct = 1 Then
                Timer9.Enabled = True
                Timer11.Enabled = False
            End If
            If direct = 2 Then
                Timer11.Enabled = True
                Timer9.Enabled = False
            End If
            Timer12.Enabled = False  'elbow to lower right, can go up or left so go left
            Timer10.Enabled = False
            Exit Sub
        End If
        '****************************************** end of the road handler
        If eatbad = 0 Then
            If BadMan2.Left Mod 32 = 0 And y2 < 0 Then
                If maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                    Timer12.Enabled = False  'open above, pacman above pacman goes up
                    Timer11.Enabled = True
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan2.Left Mod 32 = 0 And y2 > 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                Timer11.Enabled = False   'open below, pacman below badguy goes down
                Timer12.Enabled = True
                Timer9.Enabled = False
                Timer10.Enabled = False
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 > 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                Timer11.Enabled = False   'open right pacman right badguy goes right
                Timer12.Enabled = False
                Timer9.Enabled = False
                Timer10.Enabled = True
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 < 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                Timer11.Enabled = False   'open right pacman right badguy goes left
                Timer12.Enabled = False
                Timer9.Enabled = True
                Timer10.Enabled = False
                Exit Sub
            End If
        End If
        '************************************
        If eatbad = 1 Then
            If BadMan2.Left Mod 32 = 0 And y2 < 0 Then
                If maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                    Timer12.Enabled = True  'open BELOW, pacman above  goes DOWN
                    Timer11.Enabled = False
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan2.Left Mod 32 = 0 And y2 > 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                Timer11.Enabled = True  'open ABOVE, pacman below badguy goes UP
                Timer12.Enabled = False
                Timer9.Enabled = False
                Timer10.Enabled = False
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 > 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                Timer11.Enabled = False   'open LEFT pacman right badguy goes LEFT
                Timer12.Enabled = False
                Timer9.Enabled = True
                Timer10.Enabled = False
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 < 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                Timer11.Enabled = False   'open LEFT pacman LEFT badguy goes RIGHT
                Timer12.Enabled = False
                Timer9.Enabled = False
                Timer10.Enabled = True
                Exit Sub
            End If
        End If
    End Sub

    Sub reset()
        PacMan.Top = 32
        PacMan.Left = 32
        BadMan1.Top = 192
        BadMan1.Left = 256
        BadMan2.Top = 192
        BadMan2.Left = 288
        BadMan1.Image = badman1a.Image
        BadMan2.Image = badman2a.Image
        PacMan.Image = PacClosedRight.Image
        x1 = 0
        y1 = 0
        x2 = 0
        y2 = 0
        y = 0
        win = 0
        score = 0
        BADman1COUNT = 0 'necessary to have badmen come out of startbox
    End Sub

    Sub TAKETHEMOUT()
        Timer13.Enabled = True
    End Sub

    Sub TurningSub()
        If WhichKey = 40 Or 38 Then
            If PacMan.Left Mod 32 = 0 Then
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 37 Or 39 Then
            If PacMan.Top Mod 32 = 0 Then
                turning = True
                Exit Sub
            End If
        End If

        If WhichKey = 37 Then
            If maze(PacManRow, PacManColumn - 1) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 39 Then
            If maze(PacManRow, PacManColumn + 1) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 40 Then
            If maze(PacManRow + 1, PacManColumn) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 38 Then
            If maze(PacManRow - 1, PacManColumn) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If

    End Sub



    Private Sub eatGhosts_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles eatGhosts.Tick
        'handles if a ghost is eaten
        Static count
        count = count + 1
        If count = 1 Then
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'sees how far up or down form pacman to badguy1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'sees how far left or right  badguy1 is from pacman
            If Math.Abs(x1) < Math.Abs(y1) Then

            End If
            If Timer5.Enabled = True And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                Timer6.Enabled = 1
                Timer7.Enabled = 0
                Timer8.Enabled = 0
                Timer5.Enabled = 0
            End If
            If Timer6.Enabled = True And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                Timer6.Enabled = 0
                Timer7.Enabled = 0
                Timer8.Enabled = 0
                Timer5.Enabled = 1
            End If
            If Timer7.Enabled = True And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                Timer8.Enabled = 1
                Timer6.Enabled = 0
                Timer7.Enabled = 0
                Timer5.Enabled = 0
            End If
            If Timer8.Enabled = True And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then
                Timer8.Enabled = 0
                Timer6.Enabled = 0
                Timer7.Enabled = 1
                Timer5.Enabled = 0
            End If
            If Timer9.Enabled = True And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                Timer10.Enabled = 1
                Timer11.Enabled = 0
                Timer12.Enabled = 0
                Timer9.Enabled = 0
            End If
            If Timer10.Enabled = True And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                Timer10.Enabled = 0
                Timer11.Enabled = 0
                Timer12.Enabled = 0
                Timer9.Enabled = 1
            End If
            If Timer11.Enabled = True And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                Timer10.Enabled = 0
                Timer11.Enabled = 0
                Timer12.Enabled = 1
                Timer9.Enabled = 0
            End If
            If Timer12.Enabled = True And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                Timer10.Enabled = 0
                Timer11.Enabled = 1
                Timer12.Enabled = 0
                Timer9.Enabled = 0
            End If

        End If
        'label1 = Str$(PacManRow) + " " + Str$(PacManColumn)
        'If count < 2 Then Label1.Text = "eat the ghosts"
        If count <= 100 Then
            eatbad = 1
            eatbad = 1
        End If
        If count > 100 Then
            'Label1.Text = ""
            eatbad = 0
            count = 0
            eatGhosts.Enabled = False
        End If

    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        'pacman moves right
        Static x As Integer    'goes right
        'Label1.Text = Str$(PacManRow) + " " + Str$(PacManColumn)
        If Val(bead(PacManRow, PacManColumn)) <> 0 Then   'ROUTINE TO WIPE THE BEADS
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True
            End If
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = False
            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(PacManRow, PacManColumn + 1) = "1" And PacMan.Left Mod 32 = 0 Then  'IF THE NEXT BLOCK
            Timer1.Enabled = False        'IS A ONE THEN STOP THIS TIMER SO PACMAN DOESN'T GO THROUGH A
            Exit Sub                 'A WALL
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'sees how far up or down form pacman to badguy1
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'sees how far left or right  badguy1 is from pacman
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Left = PacMan.Left + 8
        PacManColumn = ((PacMan.Left + 16) \ 32) + 1   'gives column of original maze
        'label1 = PacManRow & "," & PacManColumn
        If x = 0 Then PacMan.Image = PacClosedRight.Image
        If x = 2 Then PacMan.Image = pacOpenRight.Image

        If turning = True Then
            If WhichKey = 37 Then
                Timer1.Enabled = False
                Timer2.Enabled = True
                turning = False
            End If
            If WhichKey = 39 Then turning = False 'right
            If WhichKey = 40 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer1.Enabled = False
                    Timer3.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 38 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer1.Enabled = False
                    Timer4.Enabled = True
                    turning = False
                End If
            End If
        End If
        x = x + 1
        If x = 3 Then x = -1

        'End If
        'If x = 3 Then x = -1
        'label1 = score
        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If
    End Sub

    Private Sub Timer9_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer9.Tick
        'badman2 moves left
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)   'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True
                'bad2eaten()
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)   'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                'pacdie.Enabled = True
                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And BadMan2.Left Mod 32 = 0 Then
            Timer9.Enabled = False  'stop going left
            badguys2control()          'pick a new direction
            Exit Sub
        End If
        BadMan2.Left = BadMan2.Left - 8
        BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        '******************************
        If eatbad = 0 Then
            If BadMan2.Left Mod 32 = 0 Then
                If y2 <= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then  'says pacman above clear above go above
                    Timer11.Enabled = True    'go up
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then 'goes right
                    Timer12.Enabled = True   'go down
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '****************************
        If eatbad = 1 Then
            If BadMan2.Left Mod 32 = 0 Then
                If y2 <= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then  'says pacman above clear above go above
                    Timer11.Enabled = False    'go DOWN
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer12.Enabled = True
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then 'goes right
                    Timer12.Enabled = False   'go UP
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer11.Enabled = True
                    Exit Sub
                End If
            End If
        End If
    End Sub

    Private Sub Timer7_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer7.Tick
        'badman1 moves up
        Static x As Integer

        'label1 = "timer7"
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)   'sees how far up or down form pacman to badguy1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)  'sees how far left or right  badguy1 is from pacman
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)   'sees how far up or down form pacman to badguy1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)  'sees how far left or right  badguy1 is from pacman
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                'pacdie.Enabled = True
                bad1eaten()
                Exit Sub
            End If
        End If
        If maze(BADman1ROW - 1, BADman1COLUMN) = "1" And BadMan1.Top Mod 32 = 0 Then
            Timer7.Enabled = False   'stop going up
            BADGUYS1CONTROL()          'pick a new direction
            Exit Sub
        End If
        BadMan1.Top = BadMan1.Top - 8
        BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1  'returns only integers
        '*****************************
        If eatbad = 0 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                    Timer5.Enabled = True  'go  left
                    Timer7.Enabled = False
                    Timer6.Enabled = False
                    Timer8.Enabled = False
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then  'goes right
                    Timer6.Enabled = True   'go right
                    Timer7.Enabled = False
                    Timer8.Enabled = False
                    Timer5.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '*********************************************
        If eatbad = 1 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                    Timer5.Enabled = False 'PACMAN LEFT go  RIGHT
                    Timer7.Enabled = False
                    Timer6.Enabled = True
                    Timer8.Enabled = False
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then  'goes right
                    Timer6.Enabled = False   'PACMAN RIGHT GO LEFT
                    Timer7.Enabled = False
                    Timer8.Enabled = False
                    Timer5.Enabled = True
                    Exit Sub
                End If
            End If
        End If
    End Sub

    Private Sub pacdie_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles pacdie.Tick
        'handles if pacman is killed
        Static x As Integer
        If x = 0 Or win = 1 Then
            PacMan.Image = PacBlack0.Image
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Timer5.Enabled = False
            Timer6.Enabled = False
            Timer7.Enabled = False
            Timer8.Enabled = False
            Timer9.Enabled = False
            Timer10.Enabled = False
            Timer11.Enabled = False
            Timer12.Enabled = False
        End If
        If x = 2 And win <> 1 Then PacMan.Image = PacBlack1.Image
        If x = 4 And win <> 1 Then PacMan.Image = PacBlack2.Image
        If x = 6 And win <> 1 Then PacMan.Image = PacBlack3.Image
        If x = 8 And win <> 1 Then PacMan.Image = PacBlack4.Image
        If x = 10 And win <> 1 Then PacMan.Image = PacBlack5.Image
        If x = 12 And win <> 1 Then PacMan.Image = PacBlack6.Image
        If x = 14 And win <> 1 Then PacMan.Image = PacBlack7.Image
        If x = 16 And win <> 1 Then PacMan.Image = PacBlack8.Image
        If x = 18 And win <> 1 Then PacMan.Image = PacBlack9.Image
        x = x + 1
        If x = 20 Then
            Form2.Visible = True
            Form2.Label5.Visible = True
            If win <> 1 Then
                Form2.Label5.Text = "You Lose"
                AxWindowsMediaPlayer1.URL = "Pacman-Death.mid"
                badscore = badscore + 1
            End If
            If win = 1 Then
                pacscore = pacscore + 1
                'Form1.label1 = "Pacman: " + Str$(pacscore) + "Ghosts: " + Str$(badscore)
                Form2.Label5.Text = "You Win"
                AxWindowsMediaPlayer1.URL = "Pacman-Death.mid"
            End If
        End If
        If x = 30 Then
            Form2.Visible = False
            pacdie.Enabled = False

            reset()
            loadMaze()
            x = 0
        End If

    End Sub

    Private Sub Timer8_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer8.Tick
        'badman1 moves down
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)   'sees how far up or down form pacman to badguy1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)  'sees how far left or right  badguy1 is from pacman
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)   'sees how far up or down form pacman to badguy1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)  'sees how far left or right  badguy1 is from pacman
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                'pacdie.Enabled = True
                bad1eaten()
                Exit Sub
            End If
        End If
        If maze(BADman1ROW + 1, BADman1COLUMN) = "1" And BadMan1.Top Mod 32 = 0 Then  'gets shut off here
            Timer8.Enabled = False   'stops pacman if hits a wall going down
            BADGUYS1CONTROL()
            Exit Sub
        End If
        BadMan1.Top = BadMan1.Top + 8
        BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
        '***********************
        If eatbad = 0 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                    Timer5.Enabled = True   'go left
                    Timer8.Enabled = False  'didn't work...why...because x1 > 0?
                    Timer7.Enabled = False
                    Timer6.Enabled = False
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then  'goes right
                    Timer6.Enabled = True     'go right
                    Timer8.Enabled = False    'didn't work because wall was in the way
                    Timer5.Enabled = False     '!!!!!!!!just added this
                    Timer7.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '**************************
        If eatbad = 1 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                    Timer5.Enabled = False   'go RIGHT
                    Timer8.Enabled = False  'didn't work...why...because x1 > 0?
                    Timer7.Enabled = False
                    Timer6.Enabled = True
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then  'goes right
                    Timer6.Enabled = False     'go LEFT
                    Timer8.Enabled = False    'didn't work because wall was in the way
                    Timer5.Enabled = True    '!!!!!!!!just added this
                    Timer7.Enabled = False
                    Exit Sub
                End If
            End If
        End If
    End Sub

    Private Sub Timer6_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer6.Tick
        'badman1 moves right
        Static x As Integer    'goes right
        'label1 = "Timer6"
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                'pacdie.Enabled = True
                bad1eaten()
                Exit Sub
            End If
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16)  'sees how far up or down form pacman to badguy1
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16)  'sees how far left or right  badguy1 is from pacman
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And BadMan1.Left Mod 32 = 0 Then  'IF THE NEXT BLOCK
            Timer6.Enabled = False        'IS A ONE THEN STOP THIS TIMER SO badman1 DOESN'T GO THROUGH A
            BADGUYS1CONTROL()     'stop going right and pick a new direction
            Exit Sub
        End If
        BadMan1.Left = BadMan1.Left + 8
        BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1   'gives column of original maze
        If BadMan1.Left Mod 32 = 0 Then
            'Label2 = "in"
            If y1 <= 0 And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                Timer7.Enabled = False     'go DOWN
                Timer6.Enabled = False
                Timer5.Enabled = False
                Timer8.Enabled = True
                Exit Sub
            End If
            If y1 >= 0 And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then 'goes right
                Timer8.Enabled = False     'go UP
                Timer6.Enabled = True
                Timer5.Enabled = False
                Timer7.Enabled = True
                Exit Sub
            End If
        End If
    End Sub
    Sub bad1eaten()
        BadMan1.Top = 256
        BadMan1.Left = 256
        Dim dir As Integer
        dir = Int(Rnd() * 2) + 1
        If dir = 1 Then
            Timer5.Enabled = True
            Timer6.Enabled = False
            Timer7.Enabled = False
            Timer8.Enabled = False
        End If
        If dir = 2 Then
            Timer5.Enabled = False
            Timer6.Enabled = True
            Timer7.Enabled = False
            Timer8.Enabled = False
        End If
        BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
    End Sub
    Sub bad2eaten()
        BadMan2.Top = 256
        BadMan2.Left = 288
        Dim dir As Integer
        dir = Int(Rnd() * 2) + 1
        If dir = 1 Then
            Timer9.Enabled = True
            Timer10.Enabled = False
            Timer11.Enabled = False
            Timer12.Enabled = False
        End If
        If dir = 2 Then
            Timer9.Enabled = False
            Timer10.Enabled = True
            Timer11.Enabled = False
            Timer12.Enabled = False
        End If
        BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1
    End Sub

    Private Sub Timer5_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer5.Tick
        'badman1 moves left
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'sees how far up or down form pacman to badguy1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'sees how far left or right  badguy1 is from pacman
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'sees how far up or down form pacman to badguy1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'sees how far left or right  badguy1 is from pacman
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                'pacdie.Enabled = True\
                bad1eaten()
                Exit Sub
            End If
        End If
        If maze(BADman1ROW, BADman1COLUMN - 1) = "1" And BadMan1.Left Mod 32 = 0 Then
            Timer5.Enabled = False  'stop going left
            BADGUYS1CONTROL()          'pick a new direction
            Exit Sub
        End If
        BadMan1.Left = BadMan1.Left - 8
        BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        If BadMan1.Left Mod 32 = 0 Then
            If y1 <= 0 And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then  'says pacman above clear above go above
                Timer7.Enabled = False    'go DOWN
                Timer5.Enabled = False
                Timer6.Enabled = False
                Timer8.Enabled = True
                Exit Sub
            End If
            If y1 >= 0 And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then 'goes right
                Timer8.Enabled = False  'go UP
                Timer5.Enabled = False
                Timer6.Enabled = False
                Timer7.Enabled = True
                Exit Sub
            End If
        End If
    End Sub

    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer4.Tick
        'pacman moves up
        Static x As Integer
        'Label1.Text = Str$(PacManRow) + " " + Str$(PacManColumn)
        If Val(bead(PacManRow, PacManColumn)) <> 0 Then
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True   'TIMER GOES OFF ALLOWING PACMAN TO EAT BADGUYS FOR 15 SECONDS
            End If
            'Label2 = "score=" + Str$(score)
            'Label3 = Str$(BEAD(PacManRow, PacManColumn))
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = 0
            'ImgBigDot(PacManrow, PacManColumn).Visible = 0 'wipes the beads
            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()

        If maze(PacManRow - 1, PacManColumn) = "1" And PacMan.Top Mod 32 = 0 Then
            Timer4.Enabled = False  'goes up
            Exit Sub
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'sees how far up or down form pacman to badguy1
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'sees how far left or right  badguy1 is from pacman
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Top = PacMan.Top - 8
        PacManRow = ((PacMan.Top + 16) \ 32) + 1  'returns only integers
        'label1 = PacManRow & "," & PacManColumn
        If x = 0 Then PacMan.Image = PacClosedUp.Image
        If x = 2 Then PacMan.Image = PacOpenUp.Image

        If turning = True Then
            If WhichKey = 37 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer4.Enabled = False
                    Timer2.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 39 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer4.Enabled = False
                    Timer1.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 40 Then
                Timer4.Enabled = False
                Timer3.Enabled = True
                turning = False
            End If
            If WhichKey = 38 Then turning = False
        End If
        'label1 = Maze(PacManRow, PacManColumn)
        x = x + 1
        If x = 3 Then x = -1

        'label1 = score
        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If

    End Sub

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        'pacman moves down
        Static x As Integer
        'Label1.Text = Str$(PacManRow) + " " + Str$(PacManColumn)
        If Val(bead(PacManRow, PacManColumn)) <> 0 Then
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True
            End If
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = 0
            'ImgBigDot(PacManrow, PacManColumn).Visible = 0 'wipes the beads
            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(PacManRow + 1, PacManColumn) = "1" And PacMan.Top Mod 32 = 0 Then
            Timer3.Enabled = False   'goes down
            Exit Sub
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'sees how far up or down form pacman to badguy1
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'sees how far left or right  badguy1 is from pacman
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        'label1 = score
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Top = PacMan.Top + 8
        PacManRow = ((PacMan.Top + 16) \ 32) + 1
        'label1 = PacManRow & "," & PacManColumn
        If x = 0 Then PacMan.Image = pacClosedDown.Image
        If x = 2 Then PacMan.Image = PacOpenDown.Image

        If turning = True Then
            If WhichKey = 37 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer3.Enabled = False
                    Timer2.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 39 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer3.Enabled = False
                    Timer1.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 40 Then turning = False
            If WhichKey = 38 Then
                Timer3.Enabled = False
                Timer4.Enabled = True
                turning = False
            End If
        End If
        'label1 = Maze(PacManRow, PacManColumn)
        x = x + 1
        If x = 3 Then x = -1


        'label1 = score
        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If

    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        'pacman moves left
        Static x As Integer
        'Label1.Text = Str$(PacManRow) + " " + Str$(PacManColumn)
        If Val(bead(PacManRow, PacManColumn)) <> 0 Then
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True
            End If
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = 0
            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(PacManRow, PacManColumn - 1) = "1" And PacMan.Left Mod 32 = 0 Then
            Timer2.Enabled = False  'going left
            Exit Sub
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'sees how far up or down form pacman to badguy1
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'sees how far left or right  badguy1 is from pacman
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Left = PacMan.Left - 8
        PacManColumn = ((PacMan.Left + 16) \ 32) + 1
        'label1 = PacManRow & "," & PacManColumn
        If x = 0 Then PacMan.Image = pacClosedLeft.Image
        If x = 2 Then PacMan.Image = PacOpenLeft.Image

        If turning = True Then
            If WhichKey = 37 Then turning = False
            If WhichKey = 39 Then
                Timer2.Enabled = False
                Timer1.Enabled = True
                turning = False
            End If
            If WhichKey = 40 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer2.Enabled = False
                    Timer3.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 38 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer2.Enabled = False
                    Timer4.Enabled = True
                    turning = False
                End If
            End If
            'label1 = Maze(PacManRow, PacManColumn)
        End If

        x = x + 1
        If x = 3 Then x = -1

        'label1 = score

        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If

    End Sub

    Private Sub Timer10_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer10.Tick
        'badman2 moves right
        Static x As Integer    'goes right
        'label1 = "Timer6"
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)  'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)   'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)  'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)   'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                'pacdie.Enabled = True
                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And BadMan2.Left Mod 32 = 0 Then  'IF THE NEXT BLOCK
            Timer10.Enabled = False        'IS A ONE THEN STOP THIS TIMER SO badman2 DOESN'T GO THROUGH A
            badguys2control()     'stop going right and pick a new direction
            Exit Sub
        End If
        BadMan2.Left = BadMan2.Left + 8
        BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1   'gives column of original maze
        '***********************************
        If eatbad = 0 Then
            If BadMan2.Left Mod 32 = 0 Then
                'Label2 = "in"
                If y2 <= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                    Timer11.Enabled = True     'go up
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then 'goes right
                    Timer12.Enabled = True     'go down
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '********************
        If eatbad = 1 Then
            If BadMan2.Left Mod 32 = 0 Then
                'Label2 = "in"
                If y2 <= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                    Timer11.Enabled = False     'go DOWN
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer12.Enabled = True
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then 'goes
                    Timer12.Enabled = False     'go UP
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer11.Enabled = True
                    Exit Sub
                End If
            End If
        End If

    End Sub

    Private Sub Timer13_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer13.Tick
        'handles get the bad guys out of the starting box
        Static x As Integer
        'image1 starts at 3840 left and 288 top      Code used to move the badguys out
        'image2 starts at 4320 left and 288 top      of the central fort at beginning of a level
        Static count
        'Label1.Text = count
        If x = 0 Then
            BadMan1.Image = badman1a.Image
            BadMan2.Image = badman2a.Image
        End If
        If x = 2 Then
            BadMan1.Image = badman1b.Image
            BadMan2.Image = badman2b.Image
        End If
        x = x + 1
        If x = 3 Then x = -1
        count = count + 1
        BADman1COUNT = BADman1COUNT + 1
        If count <= 12 And BadMan1.Top <= 250 Then 'badman1 comes down
            pict(178).Visible = 0  'opens gate
            BadMan1.Top = BadMan1.Top + 8
            BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
        End If
        If BadMan1.Top = 256 Then      'badman1 comes down turns right
            BadMan1.Left = BadMan1.Left + 8
            BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        End If
        If BadMan1.Left = 384 Then       'turns up on lower right corner of box
            BadMan1.Top = BadMan1.Top - 8
            BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
        End If
        If count > 49 Then '39 Then      'badman1 comes down turns right
            BadMan1.Left = BadMan1.Left + 8
            BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        End If
        If BadMan2.Left >= 261 And count > 24 Then '35 badman2 goes left
            BadMan2.Left = BadMan2.Left - 8
            BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        End If
        If BadMan2.Left = 256 Then   'badman2 goes out door
            BadMan2.Top = BadMan2.Top + 8
            BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1
        End If
        If BadMan2.Top = 256 Then  'badman2 goes left
            pict(178).Visible = True   'door closes again (becomes visible)
            BadMan2.Left = BadMan2.Left - 8
            BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        End If
        If count = 44 Then
            Timer13.Enabled = 0
            BADGUYS1CONTROL()
            count = 0
            badguys2control()
        End If

    End Sub

    Private Sub Timer12_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer12.Tick
        'badman2 moves down
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)  'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)   'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)  'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)   'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                'pacdie.Enabled = True
                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" And BadMan2.Top Mod 32 = 0 Then  'gets shut off here
            Timer12.Enabled = False   'stops pacman if hits a wall going down
            badguys2control()
            Exit Sub
        End If
        BadMan2.Top = BadMan2.Top + 8
        BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1
        '******************
        If eatbad = 0 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                    Timer9.Enabled = True   'go left
                    Timer12.Enabled = False  'didn't work...why...because x2 > 0?
                    Timer11.Enabled = False
                    Timer10.Enabled = False
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then  'goes right
                    Timer10.Enabled = True     'go right
                    Timer12.Enabled = False    'didn't work because wall was in the way
                    Timer9.Enabled = False     '!!!!!!!!just added this
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '************************
        If eatbad = 1 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                    Timer9.Enabled = False   'go RIGHT
                    Timer12.Enabled = False  'didn't work...why...because x2 > 0?
                    Timer11.Enabled = False
                    Timer10.Enabled = True
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then  'goes LEFT
                    Timer10.Enabled = True     'go LEFT
                    Timer12.Enabled = False    'didn't work because wall was in the way
                    Timer9.Enabled = False     '!!!!!!!!just added this
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If

    End Sub

    Private Sub Timer11_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer11.Tick
        'badman2 moves up
        Static x As Integer

        'label1 = "timer7"
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16) 'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16) 'sees how far up or down form pacman to badguy2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'sees how far left or right  badguy2 is from pacman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                'pacdie.Enabled = True
                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" And BadMan2.Top Mod 32 = 0 Then
            Timer11.Enabled = False  'stop going up
            badguys2control()          'pick a new direction
            Exit Sub
        End If
        BadMan2.Top = BadMan2.Top - 8
        BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1  'returns onbly integers
        '***********************************
        If eatbad = 0 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                    Timer9.Enabled = True  'go  left
                    Timer11.Enabled = False
                    Timer10.Enabled = False
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then  'goes right
                    Timer10.Enabled = True   'go right
                    Timer11.Enabled = False
                    Timer12.Enabled = False
                    Timer9.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '****************************
        If eatbad = 1 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                    Timer9.Enabled = False  'go  RIGHT
                    Timer11.Enabled = False
                    Timer10.Enabled = True
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then  'goes right
                    Timer10.Enabled = False  'go LEFT
                    Timer11.Enabled = False
                    Timer12.Enabled = False
                    Timer9.Enabled = True
                    Exit Sub
                End If
            End If
        End If
    End Sub

    Private Sub PacMan_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PacMan.Click

    End Sub

    Private Sub AxWindowsMediaPlayer1_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AxWindowsMediaPlayer1.Enter

    End Sub
End Class
