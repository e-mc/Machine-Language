.global matMult
.equ wordsize, 4

.text

matMult:

#matMult(*mat_a, rows_mat_a, cols_mat_a, *mat_b, rows_mat_b, cols_mat_b);
  #on entry stack looks like
  #esp + 24: cols_mat_b
  #esp + 20: rows_mat_b
  #esp + 16: mat_b
  #esp + 12: cols_mat_a
  #esp + 8: rows_mat_a
  #esp + 4: mat_a
  #esp: return address

#prologue
push %ebp
movl %esp, %ebp

#keep track of vars
.equ matrix_a, 2*wordsize
.equ a_rows, 3*wordsize
.equ a_cols, 4*wordsize

.equ matrix_b, 5*wordsize
.equ b_rows, 6*wordsize
.equ b_cols, 7*wordsize
#for matrix_a,b, the .equ leads to the memory address of the start of the array, 
#meaning it must be dereferenced again to move through
  
  #according to C calling convention registers EAX, ECX, and EDX will not have
  #live values in their registers. All other registers may contain live values
  #so if we want to use them we have to push their values values. 
  #The question is, where should we save these values? 
  #The answer is the stack.
  
  #esi will alternate between a and b
  #edi will hold c
  #ecx will be i. no push because ecx does not contain a live value
  #edx will be j. no push because edx does not contain a live value
  #eax will be used for temp storage. no push because eax does not contain a live value
  #ebx will be used for temp storage

  subl $wordsize * 4, %esp #make space for i on the stack
  .equ i, -wordsize
  .equ j, -2*wordsize
  .equ k, -3*wordsize
  .equ c, -4*wordsize

  push %esi
  push %ebx
  push %edi
  
  #first step is to make space for c
  movl a_rows(%ebp), %eax #EAX = a_rows
  shll $2, %eax #num_rows * sizeof(int*). a pointer is 4 bytes big so we shift eax by 2 to multiply 4
  push %eax
  call malloc
  movl %eax, c(%ebp) #c = c
  addl $wordsize, %esp #clear our argument off the stack
  
  movl $0, i(%ebp) #i = 0
  movl i(%ebp), %ecx #ECX = i
  
  #for (i = 0; i < num_rows_a; i++)
  #i < num_rows_a == i - num_rows_a < 0
  #negation i - num_rows_a >= 0
  row_for:
    cmpl a_rows(%ebp), %ecx
    jge end_row_for #i >= num_rows_a
    
    #now make the call to malloc
    movl b_cols(%ebp), %eax #put num_cols in %eax
    shll $2, %eax #num_cols * sizeof(int*). a pointer is 4 bytes big so we shift eax by 2 to multiply 4
    push %eax #put argument to malloc on the stack
    call malloc
    movl i(%ebp), %ecx #ECX = i #in case ECX was cleared
    
    movl c(%ebp), %edx
    movl %eax, (%edx, %ecx, wordsize) #c[i] points to c[i][j]
    addl $wordsize, %esp #clear our argument off the stack
    
    movl $0, j(%ebp) #j = 0
    movl j(%ebp), %edx #EDX = j
    
    #for (j = 0; j < num_cols_b; j++)
    #j < num_cols_b == j - num_cols_b < 0
    #negation: j - num_cols_b >= 0
    col_for:
      cmpl b_cols(%ebp), %edx 
      jge end_col_for #j >= num_cols_b
      
      movl c(%ebp), %esi
      movl (%esi, %ecx, wordsize), %esi #ESI = c[i]
      movl $0, (%esi, %edx, wordsize)  #c[i][j] = 0
      
      movl $0, k(%ebp) #k = 0
      movl k(%ebp), %edx #TEMP: EDX = k
      #for (k = 0; k < num_rows_b; k++)
      #k < num_rows_b == k - num_rows_b < 0
      #negation: k - num_rows_b >= 0
      pos_for:
        cmpl b_rows(%ebp), %edx  #this is num_rows
        jge end_pos_for
        
        #mat_c[i][j] += a[i][k] * b[k][j]
        #bring a[i][k] into %ebx
        movl i(%ebp), %ecx #ECX = i
        movl matrix_a(%ebp), %esi #put matrix_a into %esi
        movl (%esi, %ecx, wordsize), %esi #put a[i] into esi
        movl (%esi, %edx, wordsize), %ebx #put a[i][k] into ebx
      
        #bring b[k][j] into %ebx
        movl j(%ebp), %ecx #ECX = j
        movl matrix_b(%ebp), %esi #put b int %esi
        movl (%esi, %edx, wordsize), %esi #put b[k] into esi
        movl (%esi, %ecx, wordsize), %eax #put b[k][j] into eax
        mull %ebx #put a[i][k] * b[k][j] into eax
        movl %eax, %ebx #EBX = EAX
        
        #mat_c[i][j] += EAX
        movl i(%ebp), %ecx
        movl c(%ebp), %esi
        movl (%esi, %ecx, wordsize), %esi #put c[i] into esi
        movl j(%ebp), %edx #EDX = j
        addl %ebx, (%esi, %edx, wordsize) #c[i][j] += EBX
      
        addl $1, k(%ebp) #k++
        movl k(%ebp), %edx
        jmp pos_for
      
      end_pos_for:
      
      addl $1, j(%ebp) #j++
      movl j(%ebp), %edx #EDX = j again
      jmp col_for
      
    end_col_for:
    
    addl $1, i(%ebp) #i++
    movl i(%ebp), %ecx
    jmp row_for
  end_row_for:
      
  #put the return value in eax
  movl c(%ebp), %eax
  
  #restore registers
  pop %edi
  pop %ebx
  pop %esi
  
  
  #epilogue
  movl %ebp, %esp
  pop %ebp
  
  #go back to where we were called from
  ret
