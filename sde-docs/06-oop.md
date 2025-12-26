# DBL Object-Oriented Programming

## Namespaces

Organize code into logical groupings.

```dbl
namespace MyCompany.OrderProcessing

    class Order
        ; class definition
    endclass

endnamespace
```

### IMPORT
```dbl
import MyCompany.OrderProcessing
import System.Collections

; Now can use Order directly instead of MyCompany.OrderProcessing.Order
```

## Classes

### Basic Class
```dbl
namespace Example

class Customer

    ; Fields (private by default)
    private m_id        ,d8
    private m_name      ,string
    private m_balance   ,decimal

    ; Constructor
    public method Customer
        id      ,d8
        name    ,string
    proc
        m_id = id
        m_name = name
        m_balance = 0
    endmethod

    ; Methods
    public method AddCharge, void
        amount  ,decimal
    proc
        m_balance += amount
    endmethod

    public method GetBalance, decimal
    proc
        mreturn m_balance
    endmethod

endclass

endnamespace
```

### Access Modifiers

| Modifier | Scope |
|----------|-------|
| `public` | Accessible from anywhere |
| `private` | Only within containing class |
| `protected` | Class and derived classes |
| `internal` | Within current assembly |

## Properties

```dbl
class Product

    private m_price     ,decimal
    private m_quantity  ,int

    ; Read-write property
    public property Price, decimal
        method get
        proc
            mreturn m_price
        endmethod
        method set
        proc
            m_price = value
        endmethod
    endproperty

    ; Read-only property
    public property TotalValue, decimal
        method get
        proc
            mreturn m_price * m_quantity
        endmethod
    endproperty

    ; Auto-implemented property (simpler syntax)
    public property Name, string

endclass
```

### Property Usage
```dbl
proc
    data prod, @Product, new Product()
    prod.Price = 29.99
    prod.Name = "Widget"
    display(1, "Total: ", prod.TotalValue)
end
```

## Methods

### Instance Methods
```dbl
public method CalculateTotal, decimal
    taxRate     ,decimal
proc
    mreturn m_subtotal * (1 + taxRate)
endmethod
```

### Static Methods
```dbl
public static method CreateDefault, @Customer
proc
    mreturn new Customer(0, "Unknown")
endmethod
```

### Method Overloading
```dbl
public method Add, void
    item    ,@LineItem
proc
    ; Add single item
endmethod

public method Add, void
    items   ,[#]@LineItem
proc
    ; Add multiple items
endmethod
```

### Async Methods
```dbl
public async method FetchDataAsync, @Task<string>
    url     ,string
proc
    data client, @HttpClient, new HttpClient()
    data result, string
    result = await client.GetStringAsync(url)
    mreturn result
endmethod
```

## Constructors

```dbl
class Account

    private m_number    ,string
    private m_balance   ,decimal

    ; Default constructor
    public method Account
    proc
        m_number = ""
        m_balance = 0
    endmethod

    ; Parameterized constructor
    public method Account
        number  ,string
        initial ,decimal
    proc
        m_number = number
        m_balance = initial
    endmethod

endclass
```

## Inheritance

```dbl
class Animal
    protected m_name    ,string

    public virtual method Speak, string
    proc
        mreturn "..."
    endmethod
endclass

class Dog extends Animal

    public override method Speak, string
    proc
        mreturn "Woof!"
    endmethod

endclass

class Cat extends Animal

    public override method Speak, string
    proc
        mreturn "Meow!"
    endmethod

endclass
```

### Calling Base Class
```dbl
class SpecialCustomer extends Customer

    public method SpecialCustomer
        id      ,d8
        name    ,string
    proc
        parent(id, name)        ; Call base constructor
    endmethod

    public override method GetDiscount, decimal
    proc
        mreturn parent.GetDiscount() * 2    ; Call base method
    endmethod

endclass
```

## Interfaces

```dbl
interface IPayable
    method ProcessPayment, boolean
        amount  ,decimal
    endmethod

    property AmountDue, decimal
        method get
        endmethod
    endproperty
endinterface

class Invoice implements IPayable

    private m_amount    ,decimal
    private m_paid      ,boolean

    public method ProcessPayment, boolean
        amount  ,decimal
    proc
        if (amount >= m_amount)
        begin
            m_paid = true
            mreturn true
        end
        mreturn false
    endmethod

    public property AmountDue, decimal
        method get
        proc
            mreturn m_amount
        endmethod
    endproperty

endclass
```

## Enumerations

```dbl
enum OrderStatus
    Pending = 1
    Processing = 2
    Shipped = 3
    Delivered = 4
    Cancelled = 5
endenum
```

### Usage
```dbl
proc
    data status, OrderStatus
    status = OrderStatus.Pending

    if (status == OrderStatus.Shipped)
        display(1, "Order has shipped")
end
```

## Delegates

```dbl
delegate CalculationHandler, decimal
    value1  ,decimal
    value2  ,decimal
enddelegate
```

### Usage
```dbl
class Calculator

    public method Add, decimal
        a   ,decimal
        b   ,decimal
    proc
        mreturn a + b
    endmethod

endclass

proc
    data calc, @Calculator, new Calculator()
    data handler, @CalculationHandler

    handler = calc.Add
    display(1, handler(10, 20))     ; Outputs 30
end
```

## Events

```dbl
class Button

    public event Click, @EventHandler

    public method OnClick, void
    proc
        if (Click != ^null)
            raiseevent Click(this, new EventArgs())
    endmethod

endclass
```

### Event Handling
```dbl
proc
    data btn, @Button, new Button()

    addhandler(btn.Click, HandleClick)

    btn.OnClick()   ; Triggers handler
end

method HandleClick, void
    sender  ,@Object
    args    ,@EventArgs
proc
    display(1, "Button clicked!")
endmethod
```

## Lambda Expressions

```dbl
proc
    data numbers, [#]int, new int[#] { 1, 2, 3, 4, 5 }
    data doubled, [#]int

    ; Lambda to double each number
    doubled = numbers.Select(lambda(n) { n * 2 }).ToArray()
end
```

## Generics

```dbl
class Stack<T>

    private m_items     ,[#]T
    private m_count     ,int

    public method Push, void
        item    ,T
    proc
        ; Add to stack
    endmethod

    public method Pop, T
    proc
        ; Remove and return top
    endmethod

endclass
```

### Generic Constraints
```dbl
class Repository<T(IEntity)>    ; T must implement IEntity

    public method Save, void
        entity  ,T
    proc
        entity.Id = GetNextId()
        ; Save logic
    endmethod

endclass
```

## Object Creation

```dbl
proc
    ; Using new
    data cust, @Customer, new Customer(12345, "Acme Corp")

    ; Null check
    if (cust == ^null)
        display(1, "Customer is null")

    ; Type casting
    data obj, @Object, cust
    data cust2, @Customer, (@Customer)obj
end
```
