import turtle

n = 400

turtle.penup()
turtle.goto(-n, -n)
turtle.color("black", "gray")
turtle.begin_fill()
turtle.pendown()

while True:
    if n == 0:
        break

    turtle.forward(n)
    turtle.left(45)
    n -= 10

turtle.end_fill()
turtle.done()
