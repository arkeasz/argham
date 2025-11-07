program calculator
    use class_parser
    use ast
    use evaluator
    use symtab
    use, intrinsic :: iso_fortran_env, only: dw => real64
    implicit none
    real(kind=dw) :: result
    character(len=32) :: eq
    character(len=32) :: variables
    type(Parser) :: p
    type(ASTNode), pointer :: root
    type(Vars), pointer :: v 

    variables = "x=4.5;y=1.0"
    eq = "4x + 89y"
    p = new_parser(eq)
    v => new_symtab()
    call tokenizer(p)
    call parse(root, p)
    ! call symtab_add(v, 'y', 1.0_dw)
    ! call symtab_add(v, 'x', 4.5_dw)
    call symtab_from_string(v, variables)
    call symtab_print(v)
    if (associated(root)) then
        result = eval(root, v)
        print *, "Equation: ", eq
        print *, 'Result =', result
        ! call print_tree(root)
    else
        print *, 'No AST produced (root is null).'
    end if

end program calculator
