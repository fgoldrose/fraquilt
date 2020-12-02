# Fraquilt


Recursively generate images based on input colors and functions that manipulate those colors.

The index page uses the API to generate and display random image.

Deployed at [fraquilt.herokuapp.com](fraquilt.herokuapp.com)


<img src="https://fraquilt.s3.us-east-2.amazonaws.com/7e0a9971-73a0-48b5-886b-6b4019a2b81e.png" width="200" /> <img src="https://fraquilt.s3.us-east-2.amazonaws.com/222cb63b-b187-466d-be6c-dcff3bd8ad95.png" width="200" /> <img src="https://fraquilt.s3.us-east-2.amazonaws.com/1abe1da3-c068-4eaf-bf28-2781c6894b05.png" width="200" /> <img src="https://fraquilt.s3.us-east-2.amazonaws.com/04e86b42-5efa-44da-b599-f04ac6565164.png" width="200" />


## Generation
The image generating function is called recursively for the width and height, width and height meaning the number of generated images placed next to and above each other. During each recursive call, the colors list is manipulated according to the functions designated by that position in the functions list. 

The base case is a single pixel of the first color (r0, g0, b0). Numbers above 255 are set to 255, and numbers below 0 are set to 0.

### Functions list
Input functions consist of a series of values and operators.

Values are either color components or numbers. Color components are written with an r, g, or b followed the the number of the color (e.g. r1, g0, b3). r1 represents the red component of the 1st color, g0 the green component of the 0th color, etc.

Operators are +, -, * and /, which work as expected. Note that there is currently not support for parentheses and the order of operations is simply left to right. There must be a function for each color component of every color in the colors list, and for the width and height of the image. 

A wxh image with n+1 colors would have functions
[[[[r0 function, g0 function, b0 function], ... ,[rn function, gn function, bn function]], ...for w], ...for h]

## API
Endpoint: /api

Generate a new image with POST request sending JSON file with the format:

    {
        "iterations": <number of recursive iterations>,
        "colors": <list of initial colors formatted as [r,g,b]>,
        "functions": <functions list>
    }

### Example

    {
    "iterations":10,
    "colors":[[68,159,59],[159,74,30]],
    "functions":
        [[
          [["b1","216-r1","r0-b0"],["67/r1","b1","121-g0"]],
          [["g1","g0/2","b0"],["g1/b0","85-r1","b1*b1"]]
        ],
        [
          [["g0","130-b0","r1*40"],["b1","255","62/b1"]],
          [["227*b1","48+r0","226-110"],["224-g0","b0","g0"]]
        ]]
    }

### Limits
Images that would exceed 4000 pixels in either dimension are blocked from running. Note that depending on the width, this occurs at different recursive depths. 

There cannot be more than 10 initial colors.

