package org.siemac.metamac.statistical.operations.web.client.utils;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.navigation.shared.NameTokens;
import org.siemac.metamac.statistical.operations.navigation.shared.PlaceRequestParams;

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

    // FAMILIES

    public static PlaceRequest buildRelativeFamilyPlaceRequest(String familyCode) {
        PlaceRequest placeRequest = new PlaceRequest(NameTokens.familyPage).with(PlaceRequestParams.familyParam, familyCode);
        return placeRequest;
    }

    // OPERATIONS

    public static String getOperationIdentifierParam(PlaceManager placeManager) {
        for (PlaceRequest request : placeManager.getCurrentPlaceHierarchy()) {
            if (NameTokens.operationPage.equals(request.getNameToken())) {
                return request.getParameter(PlaceRequestParams.operationParam, null);
            }
        }
        return null;
    }

    public static PlaceRequest buildRelativeOperationPlaceRequest(String operationCode) {
        PlaceRequest placeRequest = new PlaceRequest(NameTokens.operationPage).with(PlaceRequestParams.operationParam, operationCode);
        return placeRequest;
    }

    // INSTANCES

    public static PlaceRequest buildRelativeInstancePlaceRequest(String instanceCode) {
        PlaceRequest placeRequest = new PlaceRequest(NameTokens.instancePage).with(PlaceRequestParams.instanceParam, instanceCode);
        return placeRequest;
    }

    public static List<PlaceRequest> buildAbsoluteInstancePlaceRequest(String operationCode, String instanceCode) {
        List<PlaceRequest> placeRequests = new ArrayList<PlaceRequest>();
        placeRequests.add(new PlaceRequest(NameTokens.operationListPage));
        placeRequests.add(buildRelativeOperationPlaceRequest(operationCode));
        placeRequests.add(buildRelativeInstancePlaceRequest(instanceCode));
        return placeRequests;
    }
}
