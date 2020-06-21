Public Class Form1
    Private x, y As Integer
    Private move As Boolean
    Dim operation As String
    Dim assignInput As Double = 0
    Dim found_expression As Boolean = False
    Dim fnum, snum, q, zero, equation As String
    Dim first, second As Boolean
    Dim f, s As String
    Dim changeOperator As Boolean = False
    Dim specs As Boolean = False
    Dim count As Integer
    Dim sci As Boolean
    Dim trim As Integer = 1

#Region "Move of Form"


    Private Sub Form1_MouseDown(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        move = True
        x = Windows.Forms.Cursor.Position.X - Me.Left
        y = Windows.Forms.Cursor.Position.Y - Me.Top
    End Sub

    Private Sub Form1_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseMove
        If move Then
            Me.Top = Windows.Forms.Cursor.Position.Y - y
            Me.Left = Windows.Forms.Cursor.Position.X - x
        End If
    End Sub

    Private Sub Form1_MouseUp(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseUp
        move = False
    End Sub

    Private Sub ExitToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles ExitToolStripMenuItem.Click
        Me.Close()
    End Sub



    Private Sub Panel1_MouseDown(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Panel1.MouseDown
        move = True
        x = Windows.Forms.Cursor.Position.X - Me.Left
        y = Windows.Forms.Cursor.Position.Y - Me.Top
    End Sub

    Private Sub Panel1_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Panel1.MouseMove
        If move Then
            Me.Top = Windows.Forms.Cursor.Position.Y - y
            Me.Left = Windows.Forms.Cursor.Position.X - x
        End If
    End Sub

    Private Sub Panel1_MouseUp(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Panel1.MouseUp
        move = False
    End Sub

#End Region
#Region "Number Click"

    Private Sub number_click(sender As System.Object, e As System.EventArgs) Handles Button23.Click, Button20.Click, Button19.Click, Button18.Click, Button16.Click, Button15.Click, Button14.Click, Button12.Click, Button11.Click, Button10.Click, Button22.Click
        Dim b As Button = sender
        If txtDisplay.Text = "0" Or found_expression Then
            txtDisplay.Text = String.Empty
            txtDisplay.Text = b.Text
            If specs Then
                trim_equation()
            Else

            End If
            first = False
            found_expression = False

        ElseIf b.Text = "." Then
            If Not txtDisplay.Text.Contains(".") Then
                If changeOperator Then
                    txtDisplay.Text = txtDisplay.Text + b.Text
                    'txtEquation.Text = txtDisplay.Text
                Else
                    txtDisplay.Text = txtDisplay.Text + b.Text
                    'equation = txtEquation.Text
                    'txtEquation.Text = equation + b.Text
                End If
                
            End If
        Else
            txtDisplay.Text = txtDisplay.Text + b.Text
            'txtEquation.Text = txtEquation.Text + b.Text
        End If

        changeOperator = False
        zero = ""
        second = True
        count += 1
        sci = False
        specs = False
        'first = False
    End Sub

#End Region
#Region "CE / C click"

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs) Handles Button8.Click
        If txtDisplay.Text = "0" Or count = 0 Then

        Else
            trim_equation()

        End If

        If txtEquation.Text <> "" Then
            txtDisplay.Text = "0"
            zero = "0"
            count = 0
            changeOperator = False
            'operation = ""
            second = True
            specs = False
        Else
            txtDisplay.Text = "0"
        End If
       

    End Sub

    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs) Handles Button7.Click
        txtDisplay.Text = "0"
        txtEquation.Text = ""
        assignInput = 0
        count = 0
    End Sub

#End Region
#Region "Operation Click"

    Private Sub equation_click(sender As System.Object, e As System.EventArgs) Handles Button9.Click, Button5.Click, Button17.Click, Button13.Click
        Dim b As Button = sender
        If assignInput <> 0 Then
            If changeOperator Then
                fnum = txtDisplay.Text
                operation = b.Text

                If second Then
                    If zero = "0" Then
                        txtEquation.Text = equation & "0"
                        txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                    Else
                        If trim <> 0 Then
                           
                            If sci Then
                                txtEquation.Text = equation
                                'txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                                txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                            Else
                                equation = txtDisplay.Text
                                'txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                                txtEquation.Text = equation & " " & b.Text & " "
                            End If
                            
                        Else
                            If specs Then

                            Else
                                If sci Then
                                    txtEquation.Text = equation
                                    'txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                                    txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                                Else
                                    txtEquation.Text = equation & " " & snum
                                    'txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                                    txtEquation.Text = txtEquation.Text & " " & b.Text & " "
                                End If

                            End If

                        End If
                        
                    End If
               
                End If


            Else

                Calculation()
                found_expression = True
                operation = b.Text
                assignInput = Double.Parse(txtDisplay.Text)
                If zero = "0" Then
                    equation = txtEquation.Text
                    txtEquation.Text = equation & zero & " " & operation & " "
                Else
                    'txtEquation.Text = txtEquation.Text & " " & zero & " " & operation & " "
                    If specs Then
                        equation = txtEquation.Text
                        txtEquation.Text = equation & " " & operation & " "
                    Else
                        equation = txtEquation.Text
                        txtEquation.Text = equation & " " & snum & " " & operation & " "
                    End If

                End If


            End If


        Else
            operation = b.Text
            assignInput = Double.Parse(txtDisplay.Text)
            found_expression = True
            If specs Then
                equation = txtEquation.Text
                txtEquation.Text = txtEquation.Text & " " & operation & " "
            Else
                txtEquation.Text = txtDisplay.Text & " " & operation & " "
                equation = txtDisplay.Text
            End If
            first = True
            changeOperator = True
        End If

        specs = False
        q = txtEquation.Text
        count = 0
    End Sub

    Private Sub btnEquals_Click(sender As System.Object, e As System.EventArgs) Handles btnEquals.Click
        If assignInput <> 0 Then
            snum = txtDisplay.Text

            Calculation()
            history()
            found_expression = True
            'changeOperator = True
            btnTrash.Visible = True

            assignInput = 0
            fnum = ""
            'count = 0
            second = True
            specs = False
            sci = True
            'equation = txtDisplay.Text
            'trim = 0
            
        Else

        End If

    End Sub

#End Region   
#Region "Button BackSpace"

    Private Sub btnBspace_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnBspace.Click

  
        If first Then

        Else
            If txtDisplay.Text <> "0" Then
                txtDisplay.Text = txtDisplay.Text.Remove(txtDisplay.Text.Length - 1, 1)
                'txtEquation.Text = txtEquation.Text.Remove(txtEquation.Text.Length - 1, 1)
            End If

            If txtDisplay.Text = "" Then
                txtDisplay.Text = "0"
                zero = "0"
            Else

            End If

        End If
            


    End Sub

    Sub trim_equation()
        If txtEquation.Text <> "" Then
            txtEquation.Text = txtEquation.Text.Remove(txtEquation.Text.Length - count, count)
            q = txtEquation.Text
        Else

        End If
        
    End Sub
#End Region
#Region "Other Helpers"

    Sub Calculation()
        snum = txtDisplay.Text
        Select Case operation
            Case "+"
                txtDisplay.Text = assignInput + Double.Parse(txtDisplay.Text).ToString()
            Case "-"
                txtDisplay.Text = assignInput - Double.Parse(txtDisplay.Text).ToString()
            Case "×"
                txtDisplay.Text = assignInput * Double.Parse(txtDisplay.Text).ToString()
            Case "÷"
                txtDisplay.Text = assignInput / Double.Parse(txtDisplay.Text).ToString()
        End Select
        'equation = txtEquation.Text
        trim = 0
        first = True
        assignInput = Double.Parse(txtDisplay.Text)
        changeOperator = True
        second = True
        'sci = False
        'sayIt()
    End Sub

    Sub history()

        If operation = "" Then

        Else

            rtDisplay.AppendText(" " & q & snum & "=" & vbNewLine)
            rtDisplay.ForeColor = Color.DarkBlue

            rtDisplay.AppendText(" answere : " & txtDisplay.Text & vbNewLine)
            rtDisplay.AppendText("----------------------" & vbNewLine)
            rtDisplay.SelectionStart = rtDisplay.Text.Length
            rtDisplay.ScrollToCaret()
            rtDisplay.ScrollBars = 3
            txtEquation.Text = String.Empty
        End If
        operation = ""
    End Sub

    Private Sub Label2_Click(sender As System.Object, e As System.EventArgs) Handles Label2.Click
        Me.Close()
    End Sub


    Sub sayIt()
        Dim say
        say = CreateObject("sapi.spvoice")
        say.speak(txtDisplay.Text)
    End Sub

#End Region

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim m As Double
        If count = 0 Then

        Else
            m = Convert.ToDouble(txtDisplay.Text) / Convert.ToDouble(100)
            txtDisplay.Text = System.Convert.ToString(m)

            If txtEquation.Text <> "" Then
                txtEquation.Text = txtEquation.Text & " " & txtDisplay.Text
            Else
                txtEquation.Text = txtDisplay.Text
            End If
            count = txtDisplay.Text.Length
            found_expression = True
            specs = True
            sci = True
        End If
        
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim m As Double
        If count = 0 Then

        Else
            If txtEquation.Text <> "" Then
                txtEquation.Text = txtEquation.Text & "sqrt(" & txtDisplay.Text & ")"
                count = count + 6
                'equation = txtEquation.Text
            Else
                txtEquation.Text = "sqrt(" & txtDisplay.Text & ")"
                count = count + 6
            End If

            m = Math.Sqrt(txtDisplay.Text)
            txtDisplay.Text = System.Convert.ToString(m)
            found_expression = True
            specs = True
            sci = True
        End If
        
    End Sub

    Private Sub btnTrash_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnTrash.Click
        rtDisplay.Text = String.Empty
        btnTrash.Visible = False
        rtDisplay.ScrollBars = 0
        'txtDisplay.Text = "0"
        'assignInput = 0
        Label2.Focus()
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim m As Double
        If count = 0 Then

        Else
            If txtEquation.Text <> "" Then
                txtEquation.Text = txtEquation.Text & "sqr(" & txtDisplay.Text & ")"
            Else
                txtEquation.Text = "sqr(" & txtDisplay.Text & ")"
            End If

            count = count + 5
            sci = True
            specs = True
            m = Convert.ToDouble(txtDisplay.Text) * Convert.ToDouble(txtDisplay.Text)
            txtDisplay.Text = System.Convert.ToString(m)
            found_expression = True
        End If
        
    End Sub

    Private Sub Button24_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button24.Click
        
        If count = 0 Then
            'equation = txtEquation.Text
            'txtEquation.Text = equation + txtDisplay.Text
        Else
            If txtDisplay.Text.Contains("-") Then
                
                txtDisplay.Text = txtDisplay.Text.Remove(0, 1)
            Else
                Dim clr As Integer
                clr = txtEquation.Text.Length
                txtDisplay.Text = "-" & txtDisplay.Text
            End If
        End If

    End Sub

    Private Sub ScientificCalculatorToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ScientificCalculatorToolStripMenuItem.Click
        Label1.Text = "Scientific Calculator"
    End Sub

    Private Sub StandardCalculatorToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles StandardCalculatorToolStripMenuItem.Click
        Label1.Text = "Standard Calculator"
        Me.Width = 311
        GroupBox1.Width = 301
        rtDisplay.Width = 297
        txtDisplay.Width = 292
        Label2.Left = 290
        btnTrash.Left = 285
    End Sub

End Class
