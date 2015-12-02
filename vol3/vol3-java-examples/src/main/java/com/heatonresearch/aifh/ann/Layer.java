package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;

public interface Layer {
    int getCount();

    int getTotalCount();

    int getContextCount();

    ActivationFunction getActivation();

    Layer getContextFedBy();

    void finalizeStructure(BasicNetwork theOwner, int theLayerIndex, Layer thePreviousLayer,
                           int theNeuronIndex, int theWeightIndex, int theFeedCount);

    void computeLayer();

    Layer getPreviousLayer();

    int getWeightIndex();

    int getFeedCount();

    int getNeuronIndex();
}
