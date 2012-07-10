package org.siemac.metamac.statistical.operations.web.client.utils;

import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;

public class PlaceRequestUtils {

    /**
     * Returns the first {@link PlaceRequest} in place hierarchy
     * 
     * @param placeManager
     * @return
     */
    public static String getFirstPlaceRequestInHierarchy(PlaceManager placeManager) {
        PlaceRequest request = placeManager.getCurrentPlaceHierarchy() != null ? placeManager.getCurrentPlaceHierarchy().get(0) : null;
        return request != null ? request.getNameToken() : null;
    }

}
