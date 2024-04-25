/* Angie Joya - 2322609
Emily Nuñez - 2240156*/

package object Canicas{
  type Frasco = (Int,Int) //(Numero de frasco, cantidad canicas)
  type Distr = List[Frasco] //Distribución de Frascos

  /**
    * Calcula las posibles configuraciones del frasco f con 0-c canicas
    * @param f Numero identificador de frasco
    * @param c Cantidad canicas
    * @return Lista con posibles configuraciones
    */
  def canicasPosiblesFrasco (f:Int, c:Int) : List[Frasco] ={
    val lista = for(canicas <- 0 to c) yield (f,canicas)
    lista.toList
  }

  /**
    * Calcula las posibles configuraciones de 1-n frascos con 0-c canicas
    * @param n Cantidad de frascos
    * @param c Cantidad de Canicas
    * @return Lista con posibles configuraciones
    */
  def canicasPorFrasco (n:Int, c:Int) : List[Distr] = {
    val lista = for(frascos <- 1 to n) yield (canicasPosiblesFrasco(frascos,c))
    lista.toList
  }

  /**
    * Dada una lista de canicas lc, calcula todas las combinaciones
    * posibles de canicas en los frascos
    * @param lc Lista de canicas posibles
    * @return Lista de combinaciones posibles de canicas en frascos
    */
  def mezclarLCanicas (lc: List[Distr]) : List[Distr] ={
    if(lc.isEmpty) List(List())
    else{
      val lista = for{
        cabeza <- lc.head
        cola <- mezclarLCanicas(lc.tail)
      }yield (cabeza::cola)
      lista.toList
    }            
  }     
    
  /**
    * Calcula todas las posibles maneras de distribuir m canicas en n frascos de 
    * capacidad c canicas
    * @param m cantidad total de canicas
    * @param n cantidad de frascos
    * @param c máximo de canicas por frasco
    * @return Lista de combinaciones posibles de canicas en frascos
    */
  def distribucion (m:Int, n:Int, c:Int) : List[Distr] ={
    val combinaciones = mezclarLCanicas(canicasPorFrasco(n,c))
    val lista = for{
      combinacion <- combinaciones
      if combinacion.map(_._2).sum   == m
    }yield(combinacion)
    lista.toList
  }

  /**
    * Calcula todas las maneras de dividir un conjunto de tamaño m
    * en grupos de tamaños diferentes
    * @param m Tamaño del conjunto
    * @return Lista con posibles combinaciones
    */
  def agrupaciones (m:Int): List[List[Int]]={
    val contadorMax = (m.toDouble/2.0).ceil.toInt
    val lista = for{
      cont <- 1 to contadorMax
      distri <- distribucion(m,cont,m)
      if(!distri.map(_._2).contains(0) && distri.map(_._2).distinct==distri.map(_._2))
    }yield(distri.map(_._2)).toSet
    lista.distinct.toList.map(_.toList)
  }
}