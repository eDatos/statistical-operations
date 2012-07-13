package org.siemac.metamac.statistical.operations.web.client.utils;

import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.PlaceRequestParams;

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

    public static String getOperationIdentifierParam(PlaceManager placeManager) {
        for (PlaceRequest request : placeManager.getCurrentPlaceHierarchy()) {
            if (NameTokens.operationPage.equals(request.getNameToken())) {
                return request.getParameter(PlaceRequestParams.operationParam, null);
            }
        }
        return null;
    }

}
