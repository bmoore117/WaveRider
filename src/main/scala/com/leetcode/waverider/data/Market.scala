package com.leetcode.waverider.data
import com.leetcode.waverider.utils.StateUtils
import org.deeplearning4j.gym.StepReply
import org.deeplearning4j.rl4j.mdp.MDP
import org.deeplearning4j.rl4j.space.{DiscreteSpace, ObservationSpace}
/**
  * Created by Ben on 5/20/2017.
  */
class Market extends MDP[MarketState, Int, DiscreteSpace] {

  val maxSampleId:BigInt = StateUtils.getMaxSampleId()
  val firstSampleId:BigInt = StateUtils.getFirstSampleId()

  val actionSpace = new DiscreteSpace(3)
  val state:MarketState = null

  override def newInstance(): MDP[MarketState, Int, DiscreteSpace] = {
    val market = new Market() //TODO: how parameterize this
    market
  }

  override def getActionSpace: DiscreteSpace = {
    actionSpace
  }

  override def isDone: Boolean = {
    state.sampleId == maxSampleId
  }

  override def reset(): MarketState = {
    val sample = StateUtils.getNextSample(None)
    val state = StateUtils.sampleToCluster(sample)
    new MarketState(state, firstSampleId)
  }

  override def step(action: Int): StepReply[MarketState] = ???

  override def close(): Unit = {}

  override def getObservationSpace: ObservationSpace[MarketState] = ???
}
