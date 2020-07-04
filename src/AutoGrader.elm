module AutoGrader exposing (..)

import Main exposing (Grade(..), Product)



-- Grade a product by analyzing its name
-- пиво, пивной, вино, винный → Alcohol, Bad
-- шоколад, мороженое, шоколадный, торт, пирожное, зефир, ... → Sweets, Bad
-- авокадо → Good
-- others → Neutral
