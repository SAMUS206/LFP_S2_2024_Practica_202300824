module inventario
    implicit none

    type :: inicial
        character(len=100) :: nombre
        integer :: cantidad
        real :: precio_unitario, precio_total
        character(len=50) :: ubicacion
    end type inicial
    type :: movimiento
        character(len=100) :: nombre
        integer :: cantidad
        character(len=50) :: ubicacion
    end type movimiento
    contains

    subroutine cargar_y_procesar_inventario(nombre_archivo, inventarioInicial)

        character(len=*) :: nombre_archivo
        type(inicial), allocatable :: inventarioInicial(:)
        integer :: num_equipos, ios, i
        character(len=100) :: linea, instruccion, resto
        integer :: pos
        character(len=50) :: nombre, ubicacion
        character(len=20) :: s_cantidad, s_precio_unitario
        integer :: i_delim1, i_delim2, i_delim3, cantidad
        real :: precio_unitario, precio_total
    
        ! Verificar si inventarioInicial ya está asignado y liberarla si es necesario
        if (allocated(inventarioInicial)) then
            deallocate(inventarioInicial)
        end if
        ! Abrir el archivo y contar las líneas (número de equipos)
        open(unit=10, file=nombre_archivo, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Error al abrir el archivo ", nombre_archivo
            stop
        end if
    
        num_equipos = 0
        do
            read(10, '(A)', iostat=ios) linea
            if (ios /= 0) exit
            num_equipos = num_equipos + 1
        end do
        close(10)
    
        ! Reservar memoria para el inventario
        allocate(inventarioInicial(num_equipos))
        print *, "Tamaño de inventarioInicial:", size(inventarioInicial)
    
        ! Reabrir el archivo para leer y procesar los datos
        open(unit=10, file=nombre_archivo, status='old', action='read', iostat=ios)
    
        do i = 1, num_equipos
            read(10, '(A)') linea
    
            ! Separar la instrucción del resto de la línea
            pos = INDEX(linea, ' ')
            if (pos > 0) then
                instruccion = TRIM(linea(1:pos-1))
                resto = TRIM(linea(pos+1:))
            else
                instruccion = TRIM(linea)
                resto = ''
            end if
    
            ! Procesar el resto de la línea como un equipo
            if (instruccion == 'crear_equipo')then
                print*, instruccion
    
                ! Encontrar las posiciones de los delimitadores
                i_delim1 = index(resto, ';')
                i_delim2 = index(resto(i_delim1+1:), ';') + i_delim1
                i_delim3 = index(resto(i_delim2+1:), ';') + i_delim2
    
                ! Verificar si los delimitadores son válidos
                if (i_delim1 == 0 .or. i_delim2 == 0 .or. i_delim3 == 0) then
                    print *, "Error: Delimitadores ';' no encontrados correctamente en la línea:", resto
                    cycle
                end if
    
                ! Extraer los componentes
                nombre = resto(1:i_delim1-1)
                s_cantidad = resto(i_delim1+1:i_delim2-1)
                s_precio_unitario = resto(i_delim2+1:i_delim3-1)
                ubicacion = resto(i_delim3+1:)
    
                ! Convertir cantidad y precio unitario a los tipos correctos
                read(s_cantidad, *) cantidad
                read(s_precio_unitario, *) precio_unitario
    
                ! Asignar valores al equipo
                inventarioInicial(i)%nombre = trim(nombre)
                inventarioInicial(i)%cantidad = cantidad
                inventarioInicial(i)%precio_unitario = precio_unitario
                inventarioInicial(i)%ubicacion = trim(ubicacion)
                ! Calcular el precio total
                precio_total = cantidad * precio_unitario
                inventarioInicial(i)%precio_total = precio_total
            end if
        end do
    
        close(10)
    end subroutine cargar_y_procesar_inventario

        subroutine cargar_movimientos(nombre_archivo, movimientos)
            character(len=*), intent(in) :: nombre_archivo
            type(movimiento), allocatable, intent(out) :: movimientos(:)
            integer :: num_movimientos, ios, i
            character(len=100) :: linea, instruccion, resto
            integer :: pos
    
            ! Verificar si movimientos ya está asignado y liberarlo si es necesario
            if (allocated(movimientos)) then
                deallocate(movimientos)
            end if
    
            ! Abrir el archivo y contar las líneas (número de movimientos)
            open(unit=10, file=nombre_archivo, status='old', action='read', iostat=ios)
            if (ios /= 0) then
                print *, "Error al abrir el archivo ", nombre_archivo
                stop
            end if
    
            num_movimientos = 0
            do
                read(10, '(A)', iostat=ios) linea
                if (ios /= 0) exit
                num_movimientos = num_movimientos + 1
            end do
            close(10)
    
            ! Reservar memoria para los movimientos
            allocate(movimientos(num_movimientos))
            print *, "Tamaño de movimientos:", size(movimientos)
    
            ! Reabrir el archivo para leer los datos
            open(unit=10, file=nombre_archivo, status='old', action='read', iostat=ios)
    
            do i = 1, num_movimientos
                read(10, '(A)') linea
    
                ! Separar la instrucción del resto de la línea
                pos = INDEX(linea, ' ')
                if (pos > 0) then
                    instruccion = TRIM(linea(1:pos-1))
                    resto = TRIM(linea(pos+1:))
                else
                    instruccion = TRIM(linea)
                    resto = ''
                end if
    
                ! Procesar la línea como un movimiento
                if (instruccion == 'agregar_stock'  .or. instruccion == 'eliminar_equipo') then 
                    call procesar_movimiento(resto, movimientos(i))
                end if
                
            end do
            close(10)
        end subroutine cargar_movimientos
    
        subroutine procesar_movimiento(linea, movimiento_actual)
            character(len=100) :: linea
            type(movimiento), intent(out) :: movimiento_actual
            character(len=50) :: nombre, ubicacion
            character(len=20) :: s_cantidad
            integer :: i_delim1, i_delim2, cantidad
    
            ! Encontrar las posiciones de los delimitadores
            i_delim1 = index(linea, ';')
            i_delim2 = index(linea(i_delim1+1:), ';') + i_delim1
    
            if (i_delim1 == 0 .or. i_delim2 == 0) then
                print *, "Error: Delimitadores ';' no encontrados."
                return
            end if
    
            ! Extraer los componentes
            nombre = linea(1:i_delim1-1)
            s_cantidad = linea(i_delim1+1:i_delim2-1)
            ubicacion = linea(i_delim2+1:)
    
            ! Convertir cantidad al tipo correcto
            read(s_cantidad, *) cantidad
    
            ! Asignar valores al movimiento
            movimiento_actual%nombre = trim(nombre)
            movimiento_actual%cantidad = cantidad
            movimiento_actual%ubicacion = trim(ubicacion)
            
            print*, movimiento_actual%nombre
            print*, movimiento_actual%cantidad
            print*, movimiento_actual%ubicacion
        end subroutine procesar_movimiento
    
    subroutine imprimir_inventario_consola(inventarioInicial)
        type(inicial), intent(in) :: inventarioInicial(:)
        integer :: i
    
        ! Encabezado de la tabla
        print *, "Equipo            Cantidad     Precio Unitario     Precio Total       Ubicacion"
        print *, "--------------------------------------------------------------------------"
    
        ! Recorrer y mostrar cada elemento del inventario
        do i = 1, size(inventarioInicial)
            print '(A15, I12, F18.2, F18.2, A10)', trim(inventarioInicial(i)%nombre), &
                inventarioInicial(i)%cantidad, &
                inventarioInicial(i)%precio_unitario, &
                inventarioInicial(i)%precio_total, &
                trim(inventarioInicial(i)%ubicacion)
        end do
    end subroutine imprimir_inventario_consola
    
    subroutine imprimir_inventario(inventarioInicial, nombre_archivo)
        type(inicial), intent(in) :: inventarioInicial(:)
        character(len=*), intent(in) :: nombre_archivo
        integer :: i, ios
        
        call imprimir_inventario_consola(inventarioInicial)
        ! Abrir el archivo para escribir
        open(unit=20, file=nombre_archivo, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            print *, "Error al abrir el archivo ", nombre_archivo
            stop
        end if
        
        ! Escribir el encabezado del informe
        write(20, '(A)') "Informe de Inventario:"
        write(20, '(A)') "Equipo            Cantidad     Precio Unitario       Valor Total   Ubicacion"
        write(20, '(A)') "-------------------------------------------------------------------------"
        
        ! Escribir cada línea del inventario con el formato adecuado
        do i = 1, size(inventarioInicial)
            write(20, '(A15, I12, F18.2, F18.2, A10)') trim(inventarioInicial(i)%nombre), &
                    inventarioInicial(i)%cantidad, &
                    inventarioInicial(i)%precio_unitario, &
                    inventarioInicial(i)%cantidad * inventarioInicial(i)%precio_unitario, &
                    trim(inventarioInicial(i)%ubicacion)
        end do
        ! Cerrar el archivo
        close(20)
    end subroutine imprimir_inventario

end module inventario
