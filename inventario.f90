module inventario
    implicit none

    type :: inicial
        character(len=100) :: nombre
        integer :: cantidad
        real :: precio_unitario, precio_total
        character(len=50) :: ubicacion
    end type inicial

    contains

    subroutine cargar_inventario_inicial(nombre_archivo, inventarioInicial)
        
        character(len=*) :: nombre_archivo
        type(inicial), allocatable :: inventarioInicial(:)
        integer :: num_equipos, ios, i
        character(len=100) :: linea, instruccion, resto
        integer :: pos
    
        ! Verificar si inventarioInicial ya está asignado y liberarla si es necesario
        print*, 'aqui inicia'
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
    
        ! Reabrir el archivo para leer los datos
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
    
            ! Imprimir para verificar la separación

    
            ! Procesar el resto de la línea como un equipo
            if (instruccion == 'crear_equipo')then
                print*, instruccion
                call procesar_linea(resto, inventarioInicial(i))
            end if
            if (instruccion == 'agregar_stock')then
                print*, instruccion, resto
                call agregar_stock(resto, inventarioInicial)
            end if
            if(instruccion == 'eliminar_equipo')then
                print*, instruccion, resto
                call eliminar_equipo(resto, inventarioInicial)
            end if
        end do
    
        close(10)
    end subroutine cargar_inventario_inicial

    subroutine procesar_linea(linea, equipo)
        character(len=100) :: linea
        type(inicial), intent(inout) :: equipo
        character(len=50) :: nombre, ubicacion
        character(len=20) :: s_cantidad, s_precio_unitario
        integer :: i_delim1, i_delim2, i_delim3, cantidad
        real :: precio_unitario, precio_total
    
        ! Encontrar las posiciones de los delimitadores
        i_delim1 = index(linea, ';')
        i_delim2 = index(linea(i_delim1+1:), ';') + i_delim1
        i_delim3 = index(linea(i_delim2+1:), ';') + i_delim2
    
        ! Extraer los componentes
        nombre = linea(1:i_delim1-1)
        s_cantidad = linea(i_delim1+1:i_delim2-1)
        s_precio_unitario = linea(i_delim2+1:i_delim3-1)
        ubicacion = linea(i_delim3+1:)
    
        ! Convertir cantidad y precio unitario a los tipos correctos
        read(s_cantidad, *) cantidad
        read(s_precio_unitario, *) precio_unitario
        

        ! Asignar valores al equipo
        equipo%nombre = trim(nombre)
        equipo%cantidad = cantidad
        equipo%precio_unitario = precio_unitario
        equipo%ubicacion = trim(ubicacion)
        ! Calcular el precio total
        precio_total = cantidad * precio_unitario
        equipo%precio_total = precio_total
        
    end subroutine procesar_linea

    subroutine agregar_stock(linea, inventarioInicial)
        character(len=100) :: linea
        type(inicial), intent(inout) :: inventarioInicial(:)
        character(len=50) :: nombre, ubicacion
        character(len=20) :: s_cantidad
        integer :: i_delim1, i_delim2, cantidad, i
    
        ! Encontrar las posiciones de los delimitadores
        print*, 'aqui empieza la subrutina'
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
    
        ! Buscar el equipo correcto en el inventario
        do i = 1, size(inventarioInicial)
            if (trim(nombre) == trim(inventarioInicial(i)%nombre) .and. &
                trim(ubicacion) == trim(inventarioInicial(i)%ubicacion)) then
                
                ! Actualizar la cantidad y el precio total
                inventarioInicial(i)%cantidad = inventarioInicial(i)%cantidad + cantidad
                inventarioInicial(i)%precio_total = inventarioInicial(i)%cantidad * inventarioInicial(i)%precio_unitario
                
                ! Imprimir el resultado de los cambios
                print *, "Agregado al equipo:", trim(nombre)
                print *, "Nueva cantidad:", inventarioInicial(i)%cantidad
                print *, "Nuevo precio total:", inventarioInicial(i)%precio_total
                return
            end if
        end do
    
        ! Si no se encuentra el equipo
        print *, "Error: Equipo no encontrado para agregar stock."
    end subroutine agregar_stock
    subroutine eliminar_equipo(linea, inventarioInicial)
        character(len=100) :: linea
        type(inicial), intent(inout) :: inventarioInicial(:)
        character(len=50) :: nombre, ubicacion
        character(len=20) :: s_cantidad
        integer :: i_delim1, i_delim2, cantidad, i
    
        ! Encontrar las posiciones de los delimitadores
        print*, 'aqui empieza la subrutina'
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
    
        ! Buscar el equipo correcto en el inventario
        do i = 1, size(inventarioInicial)
            if (trim(nombre) == trim(inventarioInicial(i)%nombre) .and. &
                trim(ubicacion) == trim(inventarioInicial(i)%ubicacion)) then
    
                ! Verificar si hay suficiente cantidad para eliminar
                if (inventarioInicial(i)%cantidad >= cantidad) then
                    inventarioInicial(i)%cantidad = inventarioInicial(i)%cantidad - cantidad
                    inventarioInicial(i)%precio_total = inventarioInicial(i)%cantidad * inventarioInicial(i)%precio_unitario
    
                    ! Imprimir el resultado de los cambios
                    print *, "Eliminado del equipo:", trim(nombre)
                    print *, "Nueva cantidad:", inventarioInicial(i)%cantidad
                    print *, "Nuevo precio total:", inventarioInicial(i)%precio_total
                else
                    print *, "Error: No hay suficiente cantidad para eliminar."
                end if
                return
            end if
        end do
    
        ! Si no se encuentra el equipo
        print *, "Error: Equipo no encontrado para eliminar."
    end subroutine eliminar_equipo
    
    
    subroutine imprimir_inventario(inventarioInicial, nombre_archivo)
        type(inicial), intent(in) :: inventarioInicial(:)
        character(len=*), intent(in) :: nombre_archivo
        integer :: i, ios
    
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
