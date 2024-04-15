/* Angie Joya - 2322609
Emily Nuñez - 2240156*/

package object Canicas{
    type Frasco = (Int,Int) //(Numero identificador de frasco, cantidad canicas)
    type Distr = List[Frasco] //Solución

    /**
      * Calcula las posibles configuraciones del frasco f con 0-c canicas
      * @param f Numero identificador de frasco
      * @param c Cantidad canicas
      * @return Lista con posibles configuraciones
      */
    def canicasPosiblesFrasco (f:Int, c:Int) : List[Frasco] ={
        val lista = for{canicas <- 0 to c} yield (f,canicas)
        lista.toList
    }

    /**
      * Calcula las posibles configuraciones de 0-n frascos con 0-c canicas
      * @param n Cantidad de frascos
      * @param c Cantidad de Canicas
      * @return Lista con posibles configuraciones
      */
    def canicasPorFrasco (n:Int, c:Int) : List[Distr] = {
        val lista = for{
            frascos <- 1 to n
        } yield (canicasPosiblesFrasco(frascos,c))
        lista.toList
    }

    /**
      * 
      *
      * @param lc
      * @return
      */
    def mezclarLCanicas (lc: List[Distr]) : List[Distr] ={
        (Nil)::Nil
    }

    /**
      * 
      *
      * @param m
      * @param n
      * @param c
      * @return
      */
    def distribucion (m:Int, n:Int, c:Int) : List[Distr] ={
        (Nil)::Nil
    }

    /**
      * 
      *
      * @param m
      * @return
      */
    def agrupaciones (m:Int): List[List[Int]]={
        (Nil)::Nil
    }
}
