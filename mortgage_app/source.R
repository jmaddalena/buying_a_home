calc_mo_pay = function(start_balance, int, payment){
  
  int_pay = start_balance*int/12
  prin_pay = payment - int_pay
  total_pay = int_pay + prin_pay
  
  new_balance = start_balance - prin_pay
  
  if(new_balance < 0){
    payment = start_balance + int_pay
    new_balance = 0
  }
  
  data.frame("payment" = payment, "old_balance" = start_balance, 
             "new_balance" = new_balance, "int_pay" = int_pay)
  
}
