module accion
    use inventario
    implicit none

    contains
    
    subroutine cargarArchivo(archivo)
        implicit none
        character(len=100)  :: archivo
        
        character(len=100) :: line
        integer :: ios, n, pos
        character(len=50) :: instruccion, resto
    
        open(unit=10, file = archivo, status = 'old', action = 'read', iostat = ios)
        if (ios /= 0) then
            print *, 'Error al cargar el archivo'
            stop
        end if
    
        ! Contar el número de líneas para dimensionar el arreglo 'datos'
        n = 0
        do
            read(10, '(A)', iostat = ios) line
            if (ios /= 0) exit
            n = n + 1
        end do
    
        
    
        ! Volver al inicio del archivo para leer y procesar las líneas
        rewind(10)
        n = 0
        do
            read(10, '(A)', iostat = ios) line
            if (ios /= 0) exit
    
            ! Incrementar el contador de líneas
            n = n + 1
    
            ! Encontrar la posición del primer espacio en blanco
            pos = INDEX(line, ' ')
    
            if (pos > 0) then
                ! Separar la instrucción y el resto de la línea
                instruccion = TRIM(line(1:pos-1))
                resto = TRIM(line(pos+1:))
            else
                instruccion = TRIM(line)
                resto = ''
            end if
    
            ! Guardar el "resto" en el arreglo 'datos'
            
    
            ! Imprimir para verificar
            print *, 'Instrucción: "', instruccion, '"'
            print *, 'Resto almacenado en datos(', n, '): "', resto, '"'
        end do
    
        close(10)
    end subroutine cargarArchivo
    
    
end module accion