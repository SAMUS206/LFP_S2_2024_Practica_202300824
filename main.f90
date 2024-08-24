program main
    use inventario
    implicit none

    integer :: option
    character(len=100) ::archivo
    type(inicial), allocatable :: inventarioInicial(:)
    type(movimiento), allocatable :: movimientos(:)
    

    do while (.true.)
        print *, "---------------------------------"
        print *, "practica: lenguajes formales y de programacion"
        print *, "---------------------------------"
        print *, "# Sistema de inventario"
        print *, ""
        print *, "1. Cargar inventario inicial"
        print *, "2. Cargar instrucciones de movimientos"
        print *, "3. Crear informe de inventario"
        print *, "4. Salir"

        print *, "Ingrese una opcion: "
        read *, option
        select case(option)
            case(1)
                print *, "Ingrese el nombre del archivo: "
                read *, archivo
                print *, "Cargando inventario inicial"
                call cargar_y_procesar_inventario(archivo, inventarioInicial)
                print *, "Inventario cargado con exito"
            case(2)
                print *, "Ingrese el nombre del archivo: "
                read *, archivo
                print *, "Cargando instrucciones de movimientos"
                call cargar_movimientos(archivo, movimientos, inventarioInicial)
            case(3)
                print *, "Creando informe de inventario"
                call imprimir_inventario(inventarioInicial, 'reporte.txt')  ! Imprimir el inventario cargado
            case(4)
                print *, "Saliendo del programa"
                exit
            case default
                print *, "Opcion no valida"
        end select
        print *, ""
    end do
end program main