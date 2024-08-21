module inventario
    implicit none

    type :: inicial
        character(len=100) :: nombre
        integer :: cantidad
        real :: precio_unitario
        character(len=50) :: ubicacion
    end type inicial

    contains

    subroutine cargar_inventario_inicial(nombre_archivo, inventarioInicial)
        character(len=*) :: nombre_archivo
        type(inicial), allocatable :: inventarioInicial(:)
        integer :: num_equipos, ios, i
        character(len=100) :: linea

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
            call procesar_linea(linea, inventarioInicial(i))
        end do

        close(10)
    end subroutine cargar_inventario_inicial

    subroutine procesar_linea(linea, equipo)
        character(len=100) :: linea
        type(inicial) :: equipo
        character(len=50) :: nombre, ubicacion
        character(len=20) :: s_cantidad, s_precio_unitario
        integer :: i_delim1, i_delim2, i_delim3, cantidad
        real :: precio_unitario
    
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
    end subroutine procesar_linea
    
    subroutine imprimir_inventario(inventarioInicial)
        type(inicial), intent(in) :: inventarioInicial(:)
        integer :: i

        print *, "Inventario Inicial:"
        print *, "------------------------------------------"
        do i = 1, size(inventarioInicial)
            print *, "Equipo: ", trim(inventarioInicial(i)%nombre)
            print *, "Cantidad: ", inventarioInicial(i)%cantidad
            print *, "Precio Unitario: ", inventarioInicial(i)%precio_unitario
            print *, "Ubicacion: ", trim(inventarioInicial(i)%ubicacion)
            print *, "------------------------------------------"
        end do
    end subroutine imprimir_inventario
end module inventario
