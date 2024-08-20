program main
    implicit none
    integer :: option

    do while (.true.)
        print *, "---------------------------------"
        print *, "practice: lenguajes formales y de programacion"
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
                print *, "Cargando inventario inicial"
            case(2)
                print *, "Cargando instrucciones de movimientos"
            case(3)
                print *, "Creando informe de inventario"
            case(4)
                print *, "Saliendo del programa"
                exit
            case default
                print *, "Opcion no valida"
        end select
        print *, ""
    end do
end program main
