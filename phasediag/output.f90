module Output

    use precision

    public :: AbreArquivo, EscreveArquivo, VerificaNomeArquivo, SeparaNomeExtensao

contains

    function AbreArquivo(stat, act, header, ofileName) result (u)
        character(len=*) :: header, stat, act
        character(len=512), intent(inout) :: ofileName
        character(len=512) :: fileName
        integer :: u, ios

        if (act == "write") then
            fileName = VerificaNomeArquivo(ofileName)
            ofileName = fileName
        end if

        open(unit=11, file=trim(fileName), status=trim(stat), action=trim(act), iostat=ios)
        if (ios /= 0) then
            write (*,*) 'ERRO ao abrir arquivo de saida'
            stop
        end if
        write(11,'(A)') trim(header)
        u = 11
    end function AbreArquivo

    subroutine EscreveArquivo(col1, col2, col3, col4, col5, formatStr, header, ofileName, break2lines)
        real(kr8), intent(in) :: col1(:), col2(:), col3(:), col4(:), col5(:)
        logical  , intent(in) :: break2lines
        character(len=*) :: header, formatStr, ofileName
        character(len=512) :: fileName
        integer :: i, n

        fileName = VerificaNomeArquivo(ofileName)

        n = size(col1, 1)
        write (*,*) 'Escrevendo arquivo... ', fileName
        open(unit=11, file=trim(fileName), status='replace', action='write')
        write (11, '(A)') trim(header)
        write (11,trim(formatStr)) col1(1), col2(1), col3(1), col4(1), col5(1)
        do i = 2, n
            if ((break2lines).and.(col1(i) /= col1(i-1))) then ! colocando duas quebras de linhas
                write (11,*)               ! entre dois xR diferentes
                write (11,*)               ! assim, gnuplot pode plotar usando keyword index
            end if
            write (11,trim(formatStr)) col1(i), col2(i), col3(i), col4(i), col5(i)
        end do
        close(unit=11)
    end subroutine EscreveArquivo

    function VerificaNomeArquivo(fileName) result (newFileName)
        character(len=*) :: fileName
        character(len=512) :: newFileName, fNamePrefix
        character(len=10) :: fExt, ind
        logical :: fileExists
        integer :: i

        inquire(file=trim(fileName), exist=fileExists)

        if (fileExists) then

            call SeparaNomeExtensao(fileName, fNamePrefix, fExt)
            i = 1
            do while (fileExists)
                write (ind, '(I0)') i
                write (newFileName,'(A)') trim(fNamePrefix)//'_'//trim(ind)&
                                           //'.'//trim(fExt)
                inquire(file=trim(newFileName), exist=fileExists)
                i = i + 1
            end do
        else
            write (newFileName,'(A)') trim(fileName)
        end if
    end function VerificaNomeArquivo

    subroutine SeparaNomeExtensao(fileName, fileNamePrefix, ext)
        character(len=*), intent(in) :: fileName
        character(len=512), intent(out) :: fileNamePrefix
        character(len=10), intent(out) :: ext
        integer :: ind, n

        ind = scan(fileName, '.', .true.)

        if (ind .ne. 0) then
            n = len_trim(fileName)
            fileNamePrefix = fileName(1:(ind-1))
            ext = fileName((ind+1):n)
        else
            fileNamePrefix = trim(fileName)
        end if

    end subroutine SeparaNomeExtensao

end module Output
