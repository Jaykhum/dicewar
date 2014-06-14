package main.scala.model

class Bot(override val id:Integer, game:Gamefield) extends Avatar(id)
{
  
}
//import scala.collection.mutable.ArrayBuffer
//import scala.collection.mutable.Map
//import main.scala.util.Message
//import main.scala.util.Notification
//import scala.util.Sorting
//
//class Bot(override val id:Integer, game:Gamefield) extends Avatar(id)
//{
//	
//  val attackBarrier = 3;
//  var candidateContainer:Array[Field] = null;
//  var fromLand:Field = null
//  var toLand:Field = null
//  
//  
//  def startReinforcementPhase(fieldContainer:Array[Field],world:Array[Array[Land]] )
//  {
//    var newUnits = this.getTerritories/Avatar.divider
//    if (newUnits < 3) newUnits = 3;
//    var barrierCandidate = getBarrierCandidate(fieldContainer)
//    if(barrierCandidate.length != 0)
//    {
//      setReinforcementAndBattleCandidate(barrierCandidate, world)
//      if(fromLand != null)
//      {       
//        fromLand.setArmy(fromLand.getArmy + newUnits)
//      }else{
//        var ownLandContainer = fieldContainer.filter(field => ((field.getHolder == id)))
//        var ownLandWithEnemyNeighbour = ownLandContainer.filter(field => field.checkHasEnemyNeighbour(world))
//        if(ownLandWithEnemyNeighbour.length != 0)
//        	shareUnitToLand(ownLandWithEnemyNeighbour, newUnits)
//        else
//        	shareUnitToLand(ownLandContainer, newUnits)
//      }
//    }else
//    {
//    	toLand = null
//        var ownLandContainer = fieldContainer.filter(field => ((field.getHolder == id)))
//        var ownLandWithEnemyNeighbour = ownLandContainer.filter(field => field.checkHasEnemyNeighbour(world))
//        if(ownLandWithEnemyNeighbour.length != 0){
//        	shareUnitToLand(ownLandWithEnemyNeighbour, newUnits)
//        println("Debug: Anzahl Einheiten zu verteilen " + newUnits)
//        println("Debug: Anzahl Laender " + ownLandWithEnemyNeighbour.length)
//        }
//        else
//        	shareUnitToLand(ownLandContainer, newUnits)
//    }
//    
//    
//    if(fromLand != null)
//      println("Debug: Reinforcment Land: Row: " + fromLand.position.row +  "Column: "+ fromLand.position.column)
//      
//     if(toLand != null)
//       println("Debug: Battle Land: Row: " + toLand.position.row + "Column: " + toLand.position.column)
//        
//    game.sendNotificationUI
//  }
//  
//  def shareUnitToLand(landContainer:Array[Field], units:Int)
//  {
//    var army = units
//
//    while(army != 0)
//    {
//    	for(i <- 0 until landContainer.length)
//		{
//		    if(army != 0)
//		    {
//		    	army  -= 1
//		    	landContainer(i).incArmy
//		    }else
//		      return
//		}
//        	  
//    }
//  }
//  
//  /**
//   * Candidate with barrier
//   */
//  def getBarrierCandidate(fieldContainer:Array[Field]):Array[Field] =
//  {
//     fieldContainer.filter(field => ((field.getHolder == id) && (field.army > attackBarrier)))
//  }
//  
//  /**
//   * Candidate with weaker enemy neighbour
//   */
//  def setReinforcementAndBattleCandidate(tempCandidate:Array[Field], world:Array[Array[Land]])
//  {
//      fromLand = null
//      toLand = null
//    
//	  val mapWeakNeighbour = Map[Land,Land]()
//	  for(i <- 0 to tempCandidate.length-1)
//	  {
//	    if(tempCandidate(i).checkHasWeakerEnemyNeighbour(world))
//	    {
//	      var weakestNeighbour = tempCandidate(i).getWeakestEnemyNeighbour(world)
//	      mapWeakNeighbour += tempCandidate(i) -> weakestNeighbour
//	    }
//	  }
//  	    
//	  if(mapWeakNeighbour.size != 0)
//      {
//	   val targetLand:(Land,Land) =  mapWeakNeighbour.maxBy((land:(Land,Land)) => (land._1.getArmy >  land._2.getArmy))	   
//	   fromLand = targetLand._1.asInstanceOf[Field]
//       toLand = targetLand._2.asInstanceOf[Field]     
//      }
//  }
//  
//  
//    /**
//   * Candidate with weaker enemy neighbour
//   */
//  def setBattleCandidate(fieldContainer:Array[Field], world:Array[Array[Land]])
//  {
//      fromLand = null
//      toLand = null
//    
//     var tempCandidate = getBarrierCandidate(fieldContainer)
//      if(tempCandidate.length != 0)
//      {
//    	  val mapWeakNeighbour = Map[Land,Land]()
//		  for(i <- 0 to tempCandidate.length-1)
//		  {
//		    if(tempCandidate(i).checkHasWeakerEnemyNeighbour(world))
//		    {
//		      var weakestNeighbour = tempCandidate(i).getWeakestEnemyNeighbour(world)
//		      mapWeakNeighbour += tempCandidate(i) -> weakestNeighbour
//		    }
//		  }
//	  	    
//		  if(mapWeakNeighbour.size != 0)
//	      {
//		   val targetLand:(Land,Land) =  mapWeakNeighbour.maxBy((land:(Land,Land)) => (land._1.getArmy >  land._2.getArmy))	   
//		   fromLand = targetLand._1.asInstanceOf[Field]
//	       toLand = targetLand._2.asInstanceOf[Field]     
//	      }
//      }else
//      toLand = null
//
//  }
//  
//  
//  
//  def startBattlePhase(fieldContainer:Array[Field],world:Array[Array[Land]] )
//  {
//    var counterAttack = 0
//    do{
//      
//      if(toLand != null && fromLand != null)
//      {
//        counterAttack += 1
//	      setSelectedLand(fromLand,toLand)
//	      attack(fromLand, toLand)
//      }    
//      setBattleCandidate(fieldContainer, world)
//    }while(toLand != null)
//  }
//  
//  
//  /**
//	 * Manage the count of the attacks.
//	 * @param player. Should be the current player of the game.
//	 */
//	def attack (attackLand:Field, defenseLand:Field)
//	{
//	  var ownLand = attackLand
//	  var otherLand = defenseLand
//	  var outloop = false
//	  
//	  while(!outloop)
//		  {
//		    if(ownLand.getArmy > 1)
//		    {
//			    singleAttack(ownLand, otherLand)
//			    
//			    if(otherLand.checkHolder(this) || game.gameOver)
//			      outloop = true
//			    else if (ownLand.getArmy == 1)
//			    {
//			      game.sendNotificationPlayerMessage(this,"Spieler " + this.id)
//			      game.sendNotificationMessage(Message.Info,": Angriff gescheitert")
//			    }
//		    }
//		    else
//		      outloop = true			  	
//		  }
//	  game.resetFromAndToLand
//	  resetBattle
//	      
//	}
//	
//	
//	/**
//	 * Execute a single Attack.
//	 * @param attack. Current player Land which is responsible for the attack.
//	 * @param defense. Foreign Land which try to defend.
//	 */
//	def singleAttack(attack: Field, defense: Field):Unit ={
//
//	    	  var protectLandUnit = 1
//	      
//		      var attackCountDices = attack.getArmy - protectLandUnit
//		      var defenseCountDices = defense.getArmy
//		      
//		      attackCountDices match
//		      {
//				  case 1 => attackCountDices = 1
//				  case 2 => attackCountDices = 2
//				  case _ => attackCountDices = 3
//		      }
//		      
//		      defenseCountDices match
//		      {
//				  case 1 => defenseCountDices = 1
//				  case _ => defenseCountDices = 2
//		      }
//		      
//		      var attackDice = new Array[Int](attackCountDices)
//		      var defenseDice = new Array[Int](defenseCountDices)
//		      var dice = new Dice
//		     for(i <- 0 to attackDice.length -1)
//		     {
//		       attackDice(i) = dice.roll
//		       game.sendNotificationPlayerMessage(game.avatarContainer(attack.getHolder), "Spieler " + attack.getHolder)
//		       game.sendNotificationMessage(Message.Info,": hat eine " + attackDice(i) + " gewuerfelt. ")
//		     }
//		     for(i <- 0 to defenseDice.length -1)
//		     {
//		       defenseDice(i) = dice.roll
//		       game.sendNotificationPlayerMessage(game.avatarContainer(defense.getHolder), "Spieler " + defense.getHolder)
//		       game.sendNotificationMessage(Message.Info,": hat eine " + defenseDice(i) + " gewuerfelt. ")
//		     }
//		     Sorting.quickSort(attackDice)
//		     Sorting.quickSort(defenseDice)
//		     attackDice = game.inverseArray(attackDice)
//		     defenseDice = game.inverseArray(defenseDice)
//		     
//		     // Fuer ersten Wuerfel
//		     if(attackDice(0) > defenseDice(0))
//		       defense.decArmy
//		     else
//		       attack.decArmy
//		     // Fuer zweiten Wuerfel
//		     if(defenseCountDices > 1  && attackCountDices >1)
//		     {
//		       println("Debug:Defense: " + defenseDice.length)
//		       println("Debug:Attack: " + attackDice.length)
//		       println("Debug:defenseCountDices "+ defenseCountDices)
//		       println("Debug:attackCountDices "+ attackCountDices)
//		    	if(attackDice(1) > defenseDice(1))
//		    		defense.decArmy
//		    	else
//		    		attack.decArmy
//		     }
//		    game.sendNotificationUI
//	     
//		    if(defense.getArmy == 0)
//		    {
//		        game.sendNotificationMessage(Message.Success,"Sieg!!")
//		        
//		        var lostPlayer =  defense.getHolder
//		        
//		        game.setValueForWinnerAndLoser(attack, defense)
//		        
//		        if(game.checkPlayerOutOfGame(lostPlayer) && game.checkGameOver)
//		          game.gameOver = true
//		        else
//		        {
//		          moveUnit(attack, defense, attackCountDices)
//		          game.sendNotificationUI
//		        }
//		             
//		    }
//   
//	}
//	
//	/**
//	 * Move the units from attack-land to the defense land with an assigned amount of units.
//	 * Inform the User about the status and rules of the move
//	 * @param attack. Current player Land which is responsible for the attack.
//	 * @param defense. Former Foreign Land which is now a new own land for the owner of the attack land.
//	 * @param attackCountDices. Number of Units to move.
//	 */
//	def moveUnit(attack: Field, defense: Field, attackCountDices:Int)
//	{  	    	 
//    	 if(attack.getArmy > 3)
//    	 {
//    	   var countEnemyInAttackLand = attack.getNumberOfEnemyNeighbour(game.world)
//    	   var countEnemyInDefenseLand = attack.getNumberOfEnemyNeighbour(game.world)
//    	   
//    	   if(countEnemyInAttackLand == 0 && countEnemyInDefenseLand != 0)
//    		 game.setArmyForAttackAndDefenseLand(attack, defense, attack.getArmy -1)
//    	   else if(countEnemyInAttackLand > countEnemyInDefenseLand)
//    	     game.setArmyForAttackAndDefenseLand(attack, defense, attackCountDices)
//    	   else if(countEnemyInAttackLand == 0 && countEnemyInDefenseLand == 0)
//    	     game.setArmyForAttackAndDefenseLand(attack, defense, attackCountDices)
//    	    else if(countEnemyInAttackLand < countEnemyInDefenseLand)
//    	   {
//    	     if(attack.army > attackBarrier)
//    	       game.setArmyForAttackAndDefenseLand(attack, defense, attack.army-attackBarrier)
//    	     else if(attack.army <= attackBarrier)
//    	       game.setArmyForAttackAndDefenseLand(attack, defense, attackCountDices)
//    	   }
//    	 }else 
//		    game.setArmyForAttackAndDefenseLand(attack, defense, attackCountDices)	    	 
//    	     
//	}
//	
//	
//	
//	def startTacticPhase(fieldContainer:Array[Field],world:Array[Array[Land]] )
//	{
//	   game.sendNotificationMessage(Message.Info,"Taktische Phase!")
//	   game.sendNotificationUI
//		 fromLand = null
//	     toLand = null
//	    
//	     if(game.checkPossibleForTactic(this))
//	     {
//		     val candidateStrongestMap = Map[Field,Field]()
//		     var candidateContainer = fieldContainer.filter(field => ((field.getHolder == id) && field.checkHasEnemyNeighbour(world) && field.checkHasOwnNeighbour(world)))
//		      
//		      
//			 for(i <- 0 until candidateContainer.length)
//			 {
//				   var ownNeighbourWithoutEnemyContainer = candidateContainer(i).getOwnNeighbourContainer(world).filter(field => !field.checkHasEnemyNeighbour(world))
//				   if(ownNeighbourWithoutEnemyContainer.length != 0)
//				   {
//				     var strongestNeighbour = ownNeighbourWithoutEnemyContainer.maxBy(field => field.getArmy)	
//				     candidateStrongestMap += candidateContainer(i) -> strongestNeighbour
//				   }
//				   
//			 }
//		    	 
//			  if(candidateStrongestMap.size != 0)
//			  {
//				   val targetLand:(Field,Field) = candidateStrongestMap.maxBy((field:(Field,Field)) => field._2.getArmy)
//				   if(targetLand._2.getArmy != 1)
//				   {
//				     fromLand = targetLand._2
//				     toLand = targetLand._1
//				     
//				     setSelectedLand(fromLand, toLand)
//				     game.sendNotificationUI
//				     
//			         moveUnit(fromLand, toLand, fromLand.army -1)
//			         game.sendNotificationUI
//
//				     
//				   }else
//				     game.sendNotificationMessage(Message.Info,"Taktischephase ausgesetzt.")
//				     
//				   
//			  }else if(candidateContainer.length != 0)
//			   {
//			    val candidateMap = Map[Land,Land]()
//			    val candidateUnitsMap = Map[Land,Int]()
//			    for(i <- 0 until candidateContainer.length)
//			    {
//			      if(candidateContainer(i).checkHasWeakerEnemyNeighbour(world))
//			      {
//			        var candidate = candidateContainer(i)
//			        var weakestOwnNeighbour = candidate.getOwnNeighbourContainer(world).minBy(field => field.getArmy)
//			        var strongestEnemyNeighbour = candidate.getEnemyNeighbourContainer(world).maxBy(field => field.getArmy)
//			        var unitsToMove = 0
//			        if(strongestEnemyNeighbour.army > attackBarrier)
//			        	unitsToMove = candidate.army - strongestEnemyNeighbour.army
//			        else
//			        	unitsToMove = candidate.army - attackBarrier
//			        	
//			        if(weakestOwnNeighbour.getArmy < attackBarrier && strongestEnemyNeighbour.army < candidate.army 
//			            && candidate.army > attackBarrier)
//			        {
//			          candidateMap += candidateContainer(i) -> weakestOwnNeighbour
//			          candidateUnitsMap += candidateContainer(i) -> unitsToMove
//			        }
//			      }
//			    }
//			    
//			    if(candidateUnitsMap.size != 0)
//			    {
//				    var targetUnit = candidateUnitsMap.maxBy((field:(Land,Int)) => field._2)
//				    
//				    fromLand = targetUnit._1.asInstanceOf[Field]
//				    toLand = candidateMap.get(targetUnit._1).get.asInstanceOf[Field] 
//				    
//				    setSelectedLand(fromLand, toLand)
//				    game.sendNotificationUI
//				    
//				    moveUnit(fromLand, toLand, targetUnit._2)
//				    game.sendNotificationUI
//				    
//			    }
//			    
//			   }
//			  
//			  
//	     }else
//	       game.sendNotificationMessage(Message.Info,"Taktischephase nicht moeglich.")
//     
//	}
//	
//	
//  
//	
//	
//	/**
//	 * Set the attribute fromLand and toLand so the game know which land is selected.
//	 */
//	def setSelectedLand(from:Land, to:Land)
//	{
//	  game.fromLand = from
//	  game.toLand = to
//	}
//	
//	def resetBattle
//	{
//	  toLand = null
//	}
//	
//  
//    
//}