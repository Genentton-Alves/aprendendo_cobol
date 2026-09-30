       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULA10_CADINS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 INSTRUMENTO1 PIC X(30).
       01 INSTRUMENTO2 PIC X(30).
       01 INSTRUMENTO3 PIC X(30).
       01 CONTADOR PIC 9.
       01 OPCAO PIC 9 VALUE 1.
       01 ESCOLHA PIC 9 VALUE 1.

       PROCEDURE DIVISION.

           PERFORM MENU UNTIL OPCAO = 5.
            
           STOP RUN.

       MENU.  

           
               DISPLAY "**************".
               DISPLAY "    CADASTRO".
               DISPLAY "**************".
               DISPLAY "1 - CADASTRAR".
               DISPLAY "2 - MOSTRAR".
               DISPLAY "3 - ALTERAR".
               DISPLAY "4 - EXCLUIR".
               display "5 - SAIR"
               ACCEPT OPCAO.

           PERFORM OPCAO_INVALIDA UNTIL OPCAO >=1 AND OPCAO <=5.

      
           EVALUATE OPCAO

               WHEN 1 *> ***CADASTRO***
                                    
                IF CONTADOR = 3
                    DISPLAY "CADASTRO LOTADO"

                    ELSE
                        DISPLAY "CADASTRE O INSTRUMENTO: " 

                        IF INSTRUMENTO1 = SPACES
                        DISPLAY "NOME DO INSTRUMENTO: "
                        ACCEPT INSTRUMENTO1 
                        ADD 1 TO CONTADOR
      
                    ELSE

                        IF INSTRUMENTO2 = SPACES
                        DISPLAY "NOME DO INSTRUMENTO: "
                        ACCEPT INSTRUMENTO2 
                        ADD 1 TO CONTADOR
                    
                    ELSE 

                        IF INSTRUMENTO3 = SPACES
                        DISPLAY "NOME DO INSTRUMENTO: "
                        ACCEPT INSTRUMENTO3
                        ADD 1 TO CONTADOR
                        
                        ELSE
                            DISPLAY "CADASTRO LOTADO"

                                    END-IF
                                END-IF
                            END-IF
           END-IF

               WHEN 2 *> ***MOSTRAR***
                    DISPLAY "QUAL INSTRUMENTO QUER VER?"
                    ACCEPT ESCOLHA

                    IF ESCOLHA > 3
                        DISPLAY "OPCAO INVALIDA"

                        ELSE

           EVALUATE ESCOLHA

                   WHEN 1 
                       IF INSTRUMENTO1 = SPACE
                       DISPLAY "INSTRUMENTO NAO CADASTRADO"

                       ELSE
                           DISPLAY INSTRUMENTO1
                           END-IF

                    WHEN 2
                        IF INSTRUMENTO2 = SPACE
                            DISPLAY "NAO CADASTRADO"
                       
                       ELSE 
                           DISPLAY INSTRUMENTO2
                               END-IF
                    
                    WHEN 3 
                        IF INSTRUMENTO3 = SPACE
                           DISPLAY "INSTRUMENTO NAO CADASTRADO"

                           ELSE
                               DISPLAY INSTRUMENTO3

                           END-IF
                       
                    WHEN OTHER
                        DISPLAY "OPCAO INVALIDA"
                               
           END-EVALUATE
           
           END-IF

                   WHEN 3 *> ***ALTERAR***
                        DISPLAY "QUAL INSTRUMENTO DESEJA ALTERAR?"
                        ACCEPT ESCOLHA 
                
           EVALUATE ESCOLHA

                   WHEN 1 

                       IF INSTRUMENTO1 = SPACES
                           DISPLAY "INSTRUMENTO AINDA NAO CADASTRADO"

                           ELSE 
                       DISPLAY "DIGITE O NOME DO INSTRUMENTO?" 
                       ACCEPT INSTRUMENTO1

                       END-IF

                    WHEN 2

                        IF INSTRUMENTO2 = SPACES 
                            DISPLAY "INSTRUMENTO AINDA NAO CADASTRADO"

                            ELSE 
                           DISPLAY "DIGITE O NOME DO INSTRUMENTO?" 
                           ACCEPT INSTRUMENTO2

                           END-IF

                    WHEN 3

                        IF INSTRUMENTO3 = SPACES
                            DISPLAY "INSTRUMENTO AINDA NAO CADASTRADO"

                            ELSE
                        
                               DISPLAY "DIGITE O NOME DO INSTRUMENTO?" 
                               ACCEPT INSTRUMENTO3
                            
                            END-IF
       
                    WHEN OTHER
                        DISPLAY "OPCAO INVALIDA"
                        
           END-EVALUATE


                   WHEN 4 *> ***EXCLUIR***

                       DISPLAY "QUAL INSTRUMENTO VOCE DESEJA EXCLUIR"
                       ACCEPT ESCOLHA

                       DISPLAY "ESCOLHA: " ESCOLHA
                       DISPLAY "CONTADOR: " CONTADOR

                    
                     IF ESCOLHA >=1 AND ESCOLHA <= CONTADOR

           EVALUATE ESCOLHA
                       
                    WHEN 1 

                        IF INSTRUMENTO1 = SPACES
                            DISPLAY "INSTRUMENTO NAO CADASTRADO"

                        ELSE 
                            MOVE "             " TO INSTRUMENTO1
                            SUBTRACT 1 FROM CONTADOR
                           
                        END-IF
                        
                    WHEN 2

                        IF INSTRUMENTO2 = SPACES
                        DISPLAY "INSTRUMENTO NAO CADASTRADO"

                        ELSE 
                        MOVE "             " TO INSTRUMENTO2
                        SUBTRACT 1 FROM CONTADOR
                       
                           
                        END-IF

                    WHEN 3

                        IF INSTRUMENTO3 = SPACES
                        DISPLAY "INSTRUMENTO NAO CADASTRADO"

                        ELSE
                        MOVE "             " TO INSTRUMENTO3
                        SUBTRACT 1 FROM CONTADOR
                       

                        END-IF
           END-EVALUATE

                    SUBTRACT 1 FROM CONTADOR
                    DISPLAY "INSTRUMENTO EXCLUIDO"

                    ELSE
                        DISPLAY "INSTRUMENTO NAO CADASTRADO"
               
           END-IF

               WHEN 5 *> ***SAIR***
                   DISPLAY "SAIR"
        
           END-EVALUATE.
       
       OPCAO_INVALIDA.

               DISPLAY "ESCOLHA OUTRA OPCAO".
               ACCEPT OPCAO.
