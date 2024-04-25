Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Text

Module Module1
    Private Const crcPoly As UShort = &H1021
    Private ReadOnly crcTable(255) As UShort
    Private Enum SubMode As Integer
        None = -1
        LibCryptv1 = &H0
        LibCryptv2 = &H1
        SecuROMv1 = &H2
        SecuROMv2 = &H3
        SecuROMv3a = &H4
        SecuROMv3b = &H5
        SecuROMv3c = &H6
        SecuROMv4 = &H7
    End Enum

    Private ReadOnly ModeNames As New Dictionary(Of String, Integer) From {
        {"libcrypt1", SubMode.LibCryptv1},
        {"libcrypt2", SubMode.LibCryptv2},
        {"securom1", SubMode.SecuROMv1},
        {"securom2", SubMode.SecuROMv2},
        {"securom3a", SubMode.SecuROMv3a},
        {"securom3b", SubMode.SecuROMv3b},
        {"securom3c", SubMode.SecuROMv3c},
        {"securom4", SubMode.SecuROMv4}
    }

    Sub Main()
        Dim args = Environment.GetCommandLineArgs()
        Dim badArgs As Boolean = False
        Dim badArgError As String = ""
        Dim badArgList = New List(Of String)
        Dim missingArgList = New List(Of String)

        If args.Length < 2 Then
            badArgs = True
        End If

        If Not badArgs Then
            Dim mode As SubMode = SubMode.None
            Dim mdf As Boolean = False
            Dim fname As String = ""

            For i = 1 To args.Length - 1
                If args(i) = "-d" Or args(i) = "--drm" Then
                    If i + 1 < args.Length Then
                        i += 1
                        Dim ModeName As String = args(i)
                        If ModeNames.ContainsKey(ModeName) Then
                            mode = ModeNames.Item(ModeName)
                        Else
                            Dim ModeNameList As String = String.Join(", ", ModeNames.[Select](Function(kv) "'" & kv.Key.ToString() & "'"))
                            badArgs = True
                            badArgError = $"argument {args(i - 1)}: invalid choice: '{ModeName}' (choose from {ModeNameList})"
                            Exit For
                        End If
                    Else
                        badArgs = True
                        badArgError = $"argument {args(i)}: expected one argument"
                        Exit For
                    End If
                ElseIf args(i) = "-m" Or args(i) = "--mdf" Then
                    mdf = True
                ElseIf fname = "" Then
                    fname = args(i)
                Else
                    badArgs = True
                    badArgList.Add(args(i))
                End If
            Next

            If fname = "" Then
                badArgs = True
                missingArgList.Add("inFile")
            End If

            If Not badArgs Then
                If Not fname.EndsWith(".bin") AndAlso Not fname.EndsWith(".img") Then
                    Environment.Exit(1)
                End If

                InitCrcTable()
                Dim pregap As Integer = MakeSub(fname)
                PatchSub(fname, pregap, mode)
                If mdf Then
                    BinToMdf(fname)
                End If
            End If
        End If

        If badArgs Then
            Dim ModeNameList As String = String.Join(",", ModeNames.[Select](Function(kv) kv.Key.ToString()))
            Console.WriteLine($"usage: SBIConverter.exe [-d {{{ModeNameList}}}] [-m] inFile")
            If badArgError <> "" Then
                Console.WriteLine("")
                Console.WriteLine($"error: {badArgError}")
            ElseIf missingArgList.Count > 0 Then
                Console.WriteLine("")
                Console.WriteLine($"error: the following arguments are required: {String.Join(" ", missingArgList.ToArray())}")
            ElseIf badArgList.Count > 0 Then
                Console.WriteLine("")
                Console.WriteLine($"error: unrecognized argument: {String.Join(" ", badArgList.ToArray())}")
            End If
            Environment.Exit(1)
        End If
    End Sub

    Private Sub InitCrcTable()
        For i As Integer = 0 To 255
            Dim r As UShort = CUShort(i << 8)
            For j As Integer = 0 To 7
                If (r And &H8000) <> 0 Then
                    r = CUShort((r << 1) Xor crcPoly)
                Else
                    r = CUShort(r << 1)
                End If
            Next
            crcTable(i) = CUShort(r And &HFFFF)
        Next
    End Sub

    Private Function Crc16(data As Byte()) As UShort
        Dim ret As UShort = 0
        For Each b As Byte In data
            Dim idx As Integer = ((ret >> 8) Xor b) And &HFF
            ret = CUShort((ret << 8) Xor crcTable(idx))
        Next
        Return CUShort(Not ret And &HFFFF)
    End Function

    Private Function SectorsToMsf(sectors As Integer) As Byte()
        Dim m As Integer = sectors \ 75 \ 60
        Dim s As Integer = (sectors \ 75) Mod 60
        Dim f As Integer = sectors Mod 75

        Dim msf(2) As Byte
        msf(0) = Convert.ToByte(m.ToString, 16)
        msf(1) = Convert.ToByte(s.ToString, 16)
        msf(2) = Convert.ToByte(f.ToString, 16)

        Return msf
    End Function

    Private Function MsfToSectors(msf As Byte()) As Integer
        Dim m As Integer = CInt(msf(0).ToString("x2"))
        Dim s As Integer = CInt(msf(1).ToString("x2"))
        Dim f As Integer = CInt(msf(2).ToString("x2"))

        Return m * 75 * 60 + s * 75 + f
    End Function

    Private Function BytesToHexString(b() As Byte) As String
        Return BitConverter.ToString(b).Replace("-", "").ToLower()
    End Function

    Private Function GetBytes(source() As Byte, index As Integer, length As Integer) As Byte()
        Dim dest(length - 1) As Byte

        Array.Copy(source, index, dest, 0, length)

        Return dest
    End Function

    Private Function FileReadBytes(f As FileStream, start As Integer, length As Integer) As Byte()
        Dim data(length - 1) As Byte
        f.Seek(start, SeekOrigin.Begin)
        f.Read(data, 0, length)

        Return data
    End Function

    Private Function FileReadBytes(f As FileStream, length As Integer) As Byte()
        Dim data(length - 1) As Byte
        f.Read(data, 0, length)

        Return data
    End Function

    Private Sub FileWriteBytes(f As FileStream, b() As Byte, start As Integer)
        f.Seek(start, SeekOrigin.Begin)
        f.Write(b, 0, b.Length)
    End Sub

    Private Sub FileWriteBytes(f As FileStream, b() As Byte)
        f.Write(b, 0, b.Length)
    End Sub

    Private Sub FillArray(b() As Byte, value As Byte, index As Integer, length As Integer)
        For i = index To length - 1
            b(i) = value
        Next
    End Sub

    Private Function CRCToBytes(crc As UShort) As Byte()
        Dim bytes = BitConverter.GetBytes(crc)
        Array.Reverse(bytes)

        Return bytes
    End Function

    Private Function MakeSub(fname As String) As Integer
        Dim sectors As Integer
        Dim pregap As Integer

        Using f As New FileStream(fname, FileMode.Open, FileAccess.Read)
            Dim data = FileReadBytes(f, 0, 16)
            pregap = MsfToSectors(GetBytes(data, 12, 3))
            f.Seek(0, SeekOrigin.End)
            sectors = CInt(f.Position \ 2352)
            Console.WriteLine($"Generating SUB for {sectors} sectors with {pregap} pregap")
        End Using

        Using f As New FileStream(fname.Substring(0, fname.Length - 4) & ".sub", FileMode.Create, FileAccess.Write)
            For i As Integer = 0 To sectors - 1
                Dim data(95) As Byte

                ' Write channel P.
                If i = 0 OrElse (sectors - i) < 150 Then
                    FillArray(data, &HFF, 0, 12)
                End If

                ' Write channel Q mode 1.
                Array.Copy(New Byte() {&H41, &H1, &H1}, 0, data, 12, 3)
                Array.Copy(SectorsToMsf(i), 0, data, 15, 3)
                Array.Copy(SectorsToMsf(i + pregap), 0, data, 19, 3)
                Dim crc As UShort = Crc16(GetBytes(data, 12, 10))
                CRCToBytes(crc).CopyTo(data, 22)

                FileWriteBytes(f, data)
            Next
        End Using

        Return pregap
    End Function

    Private Sub PatchSub(fname As String, pregap As Integer, mode As SubMode)
        Dim filepart = Path.GetFileNameWithoutExtension(fname)

        Console.WriteLine("Patching SUB with SBI")

        Using f As FileStream = File.OpenRead(filepart & ".sbi"),
            f2 As FileStream = File.Open(filepart & ".sub", FileMode.Open, FileAccess.ReadWrite, FileShare.None)
            Dim magic = FileReadBytes(f, 0, 4)
            If magic.SequenceEqual(Encoding.ASCII.GetBytes("SBI" & vbNullChar)) = False Then
                Throw New Exception("Wrong header magic in SBI file.")
            End If

            f.Seek(0, SeekOrigin.End)
            Dim fsize As Long = f.Position
            f.Seek(4, SeekOrigin.Begin)
            Dim numPatches As Integer = CInt((fsize - 4) / 14)
            Dim patches As New List(Of (Integer, Byte()))

            If numPatches < 2 Then
                Throw New Exception($"Number of patches too small for SecuROM ({numPatches}).")
            End If

            For i As Integer = 0 To numPatches - 1
                Dim msf = FileReadBytes(f, 4)
                If msf(3) <> 1 Then
                    Throw New Exception($"Bad entry in SBI at {f.Position - 4:x8}.")
                End If
                Dim patchData = FileReadBytes(f, 10)
                Dim sectors As Integer = MsfToSectors(msf) - pregap
                patches.Add((sectors, patchData))
            Next

            If mode = SubMode.None Then
                ' Autodetect protection.
                ' Don't detect LibCrypt since it has two versions which can't be
                ' easily told apart.
                If patches(0).Item1 = -1 AndAlso patches(1).Item1 = 5000 Then
                    Console.WriteLine("SecuROM v4+")
                    mode = SubMode.SecuROMv4
                ElseIf patches(0).Item1 = 5000 AndAlso patches(1).Item1 = 5001 Then
                    Console.WriteLine("SecuROM v2")
                    mode = SubMode.SecuROMv2
                ElseIf patches(0).Item1 = -1 AndAlso patches(1).Item1 = 0 AndAlso patches(9).Item1 = 5000 Then
                    Dim rmsf As Byte() = GetBytes(patches(0).Item2, 3, 3)
                    If rmsf.SequenceEqual(New Byte() {&H37, &H10, &H45}) Then
                        Console.WriteLine("SecuROM v3 (variant 1)")
                        mode = SubMode.SecuROMv3a
                    ElseIf rmsf.SequenceEqual(New Byte() {&H0, &H0, &H0}) Then
                        Console.WriteLine("SecuROM v3 (variant 2)")
                        mode = SubMode.SecuROMv3b
                    ElseIf rmsf(2) = &HFF Then
                        ' Need more samples.
                        Console.WriteLine("SecuROM v3 (variant 3)")
                        mode = SubMode.SecuROMv3c
                    End If
                ElseIf patches(0).Item1 >= 40000 AndAlso patches(0).Item1 < 45800 Then
                    Console.WriteLine("SecuROM v1")
                    mode = SubMode.SecuROMv1
                End If
            End If

            If mode = SubMode.None Then
                Throw New Exception("Unknown protection!!!")
            End If

            For i As Integer = 0 To numPatches - 1
                Dim sectors As Integer = patches(i).Item1
                Dim patchData() As Byte = patches(i).Item2

                If sectors < 0 Then ' Skip track 1 pregap.
                    Continue For
                End If

                Dim prevOffset = (sectors - 1) * 96 + 12
                Dim currentOffset = sectors * 96 + 12

                Select Case mode
                    Case SubMode.LibCryptv1
                        ' Calculate CRC-16 checksum of original data.
                        Dim crc As UShort = Crc16(FileReadBytes(f2, currentOffset, 10))

                        FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.
                        FileWriteBytes(f2, CRCToBytes(CUShort(crc Xor &H8001))) ' Write XOR'd checksum.

                        Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4} ^ 0x8001")

                    Case SubMode.LibCryptv2
                        ' Calculate CRC-16 checksum of patched data.
                        Dim crc As UShort = Crc16(patchData)

                        FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.
                        FileWriteBytes(f2, CRCToBytes(CUShort(crc Xor &H80))) ' Write XOR'd checksum.

                        Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4} ^ 0x80")

                    Case SubMode.SecuROMv1
                        ' Calculate CRC-16 checksum of patched data.
                        Dim crc As UShort = Crc16(patchData)
                        FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.

                        If (i + 1) Mod 9 = 0 Then
                            FileWriteBytes(f2, CRCToBytes(CUShort(crc Xor &H80))) ' Write XOR'd checksum.
                            Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4} ^ 0x80")
                        Else
                            FileWriteBytes(f2, CRCToBytes(crc)) ' Write normal checksum.
                            Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4}")
                        End If

                    Case SubMode.SecuROMv2, SubMode.SecuROMv3b, SubMode.SecuROMv3c
                        If (i + 1) Mod 9 = 0 Then
                            ' Calculate CRC-16 checksum of original data.
                            Dim crc As UShort = Crc16(FileReadBytes(f2, currentOffset, 10))

                            FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.
                            FileWriteBytes(f2, CRCToBytes(CUShort(crc Xor &H8001))) ' Write XOR'd checksum.

                            Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4} ^ 0x8001")
                        Else
                            ' Calculate CRC-16 checksum of patched data.
                            Dim crc As UShort = Crc16(patchData)

                            FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.
                            FileWriteBytes(f2, CRCToBytes(crc)) ' Write normal checksum.
                            Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4}")
                        End If

                    Case SubMode.SecuROMv3a
                        If (i + 1) Mod 9 = 0 Then
                            ' Calculate CRC-16 checksum of the *previous* sector.
                            Dim crc As UShort = Crc16(FileReadBytes(f2, prevOffset, 10))

                            FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.
                            FileWriteBytes(f2, CRCToBytes(CUShort(crc Xor &H8001))) ' Write XOR'd checksum.
                            Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4} ^ 0x8001")
                        Else
                            ' Calculate CRC-16 checksum of patched data.
                            Dim crc As UShort = Crc16(patchData)

                            FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.
                            FileWriteBytes(f2, CRCToBytes(crc)) ' Write normal checksum.
                            Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4}")
                        End If

                    Case SubMode.SecuROMv4
                        ' Calculate CRC-16 checksum of original data.
                        Dim crc As UShort = Crc16(FileReadBytes(f2, currentOffset, 10))

                        FileWriteBytes(f2, patchData, currentOffset) ' Write patched data.
                        FileWriteBytes(f2, CRCToBytes(CUShort(crc Xor &H8001))) ' Write XOR'd checksum.

                        Console.WriteLine($"{sectors:D}: {BytesToHexString(patchData)} CRC 0x{crc:x4} ^ 0x8001")
                End Select
            Next
        End Using
    End Sub

    Private Sub BinToMdf(fname As String)
        Dim filepart = Path.GetFileNameWithoutExtension(fname)

        Console.WriteLine($"Building {filepart & ".mdf"}")

        Using f As FileStream = File.OpenRead(fname),
              f2 As FileStream = File.OpenRead(filepart & ".sub"),
              out As FileStream = File.Create(filepart & ".mdf")
            Dim data(2351) As Byte
            Dim bytesRead = f.Read(data, 0, data.Length)
            While bytesRead > 0
                ' Write data track.
                out.Write(data, 0, bytesRead)

                ' Convert subcodes.
                Dim subs = FileReadBytes(f2, 96)
                Dim outSubs(95) As Byte
                For i As Integer = 0 To 7
                    For j As Integer = 0 To 95
                        Dim pos As Integer = (96 * i + j) \ 8
                        Dim shift As Integer = 7 - ((96 * i + j) Mod 8)
                        Dim flag As Boolean = (subs(pos) And (1 << shift)) <> 0
                        If flag Then
                            pos = j
                            shift = 7 - i
                            outSubs(pos) = CByte(outSubs(pos) Or (1 << shift))
                        End If
                    Next
                Next
                FileWriteBytes(out, outSubs)

                bytesRead = f.Read(data, 0, data.Length)
            End While
        End Using
    End Sub
End Module

Module ByteArrayExtensions
    <Extension()>
    Public Function CompareTo(b1() As Byte, b2() As Byte, Optional IgnoreLength As Boolean = False) As Boolean
        If Not IgnoreLength AndAlso b1.Length <> b2.Length Then
            Return False
        End If

        Dim Length = Math.Min(b1.Length, b2.Length)

        For Counter = Length - 1 To 0 Step -1
            If b1(Counter) <> b2(Counter) Then
                Return False
            End If
        Next

        Return True
    End Function
End Module
