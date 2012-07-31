package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import static org.junit.Assert.fail;

import java.math.BigInteger;

import org.joda.time.DateTime;
import org.siemac.metamac.common.metadata.rest.internal.v1_0.domain.Configuration;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.SimpleItemsNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.ProcStatus;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Status;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.service.StatisticalOperationsRestInternalFacadeV10Test;

public class StatisticalOperationsRestMocks {

    public static Operation mockOperation1(String baseApi) {
        return mockOperation(baseApi, "1", ProcStatus.PUBLISH_INTERNALLY);
    }

    public static Operation mockOperation2(String baseApi) {
        return mockOperation(baseApi, "2", ProcStatus.PUBLISH_INTERNALLY);
    }

    public static Operation mockOperation3(String baseApi) {
        return mockOperation(baseApi, "3", ProcStatus.PUBLISH_EXTERNALLY);
    }

    public static Operation mockOperation4(String baseApi) {
        return mockOperation(baseApi, "4", ProcStatus.PUBLISH_INTERNALLY);
    }

    public static Operation mockOperation5(String baseApi) {
        return mockOperation(baseApi, "5", ProcStatus.PUBLISH_INTERNALLY);
    }

    public static Operation mockOperation6(String baseApi) {
        return mockOperation(baseApi, "6", ProcStatus.PUBLISH_EXTERNALLY);
    }

    public static Family mockFamily1(String baseApi) {
        return mockFamily(baseApi, "1", ProcStatus.PUBLISH_INTERNALLY);
    }

    public static Family mockFamily2(String baseApi) {
        return mockFamily(baseApi, "2", ProcStatus.PUBLISH_EXTERNALLY);
    }

    public static Instance mockInstance1(String baseApi) {
        return mockInstance(baseApi, "1", "operation1", ProcStatus.PUBLISH_INTERNALLY);
    }

    public static ResourcesPagedResult mockOperationsPagedResult(String baseApi, String limit, String offset) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);
        pagedResult.setTotal(BigInteger.valueOf(10));
        if (limit == null && (offset == null || "0".equals(offset))) {
            pagedResult.getItems().add(mockOperation1Resource(baseApi));
            pagedResult.getItems().add(mockOperation2Resource(baseApi));
            pagedResult.getItems().add(mockOperation3Resource(baseApi));
            pagedResult.getItems().add(mockOperation4Resource(baseApi));
            pagedResult.getItems().add(mockOperation5Resource(baseApi));
            pagedResult.getItems().add(mockOperation6Resource(baseApi));
            pagedResult.getItems().add(mockOperation7Resource(baseApi));
            pagedResult.getItems().add(mockOperation8Resource(baseApi));
            pagedResult.getItems().add(mockOperation9Resource(baseApi));
            pagedResult.getItems().add(mockOperation10Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(25));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            pagedResult.getItems().add(mockOperation1Resource(baseApi));
            pagedResult.getItems().add(mockOperation2Resource(baseApi));
            pagedResult.getItems().add(mockOperation3Resource(baseApi));
            pagedResult.getItems().add(mockOperation4Resource(baseApi));
            pagedResult.getItems().add(mockOperation5Resource(baseApi));
            pagedResult.getItems().add(mockOperation6Resource(baseApi));
            pagedResult.getItems().add(mockOperation7Resource(baseApi));
            pagedResult.getItems().add(mockOperation8Resource(baseApi));
            pagedResult.getItems().add(mockOperation9Resource(baseApi));
            pagedResult.getItems().add(mockOperation10Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(1000));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pagedResult.getItems().add(mockOperation1Resource(baseApi));
            pagedResult.getItems().add(mockOperation2Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(baseApi + "/operations?limit=2&offset=2");
            pagedResult.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pagedResult.getItems().add(mockOperation3Resource(baseApi));
            pagedResult.getItems().add(mockOperation4Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/operations?limit=2&offset=0");
            pagedResult.setNextLink(baseApi + "/operations?limit=2&offset=4");
            pagedResult.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "8".equals(offset)) {
            pagedResult.getItems().add(mockOperation9Resource(baseApi));
            pagedResult.getItems().add(mockOperation10Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/operations?limit=2&offset=6");
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return pagedResult;
    }

    public static ResourcesPagedResult mockOperationsPagedResult(String baseApi, String limit, String offset, String query) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);
        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1;
        String querySupported2 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM;
        if (querySupported1.equals(query)) {
            pagedResult.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                pagedResult.getItems().add(mockOperation1Resource(baseApi));
                pagedResult.getItems().add(mockOperation10Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(0));
                pagedResult.setLimit(BigInteger.valueOf(25));
                pagedResult.setFirstLink(null);
                pagedResult.setPreviousLink(null);
                pagedResult.setNextLink(null);
                pagedResult.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                pagedResult.getItems().add(mockOperation1Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                pagedResult.setFirstLink(null);
                pagedResult.setPreviousLink(null);
                pagedResult.setNextLink(baseApi + "/operations?query=" + query + "&limit=1&offset=1");
                pagedResult.setLastLink(baseApi + "/operations?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                pagedResult.getItems().add(mockOperation3Resource(baseApi));
                pagedResult.getItems().add(mockOperation4Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                pagedResult.setFirstLink(baseApi + "/operations?query=" + query + "&limit=1&offset=0");
                pagedResult.setPreviousLink(baseApi + "/operations?query=" + query + "&limit=1&offset=0");
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else if (querySupported2.equals(query)) {
            pagedResult.setTotal(BigInteger.valueOf(1));
            if (limit == null && (offset == null || "0".equals(offset))) {
                pagedResult.getItems().add(mockOperation1Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(0));
                pagedResult.setLimit(BigInteger.valueOf(25));
                pagedResult.setFirstLink(null);
                pagedResult.setPreviousLink(null);
                pagedResult.setNextLink(null);
                pagedResult.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return pagedResult;
    }

    public static ResourcesPagedResult mockOperationsPagedResultByFamily1(String baseApi, String limit, String offset) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);
        pagedResult.setTotal(BigInteger.valueOf(6));
        if (limit == null && (offset == null || "0".equals(offset))) {
            pagedResult.getItems().add(mockOperation1Resource(baseApi));
            pagedResult.getItems().add(mockOperation2Resource(baseApi));
            pagedResult.getItems().add(mockOperation3Resource(baseApi));
            pagedResult.getItems().add(mockOperation4Resource(baseApi));
            pagedResult.getItems().add(mockOperation5Resource(baseApi));
            pagedResult.getItems().add(mockOperation6Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(25));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            pagedResult.getItems().add(mockOperation1Resource(baseApi));
            pagedResult.getItems().add(mockOperation2Resource(baseApi));
            pagedResult.getItems().add(mockOperation3Resource(baseApi));
            pagedResult.getItems().add(mockOperation4Resource(baseApi));
            pagedResult.getItems().add(mockOperation5Resource(baseApi));
            pagedResult.getItems().add(mockOperation6Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(1000));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pagedResult.getItems().add(mockOperation1Resource(baseApi));
            pagedResult.getItems().add(mockOperation2Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            pagedResult.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pagedResult.getItems().add(mockOperation3Resource(baseApi));
            pagedResult.getItems().add(mockOperation4Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            pagedResult.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=4");
            pagedResult.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            pagedResult.getItems().add(mockOperation5Resource(baseApi));
            pagedResult.getItems().add(mockOperation6Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "7".equals(offset)) {
            // no results
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return pagedResult;
    }

    public static ResourcesPagedResult mockOperationsPagedResultByFamily2(String baseApi, String limit, String offset) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);
        pagedResult.setTotal(BigInteger.valueOf(4));
        if ("1".equals(limit) && "2".equals(offset)) {
            pagedResult.getItems().add(mockOperation8Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(2));
            pagedResult.setLimit(BigInteger.valueOf(1));
            pagedResult.setFirstLink(baseApi + "/families/family2/operations?limit=1&offset=0");
            pagedResult.setPreviousLink(baseApi + "/families/family2/operations?limit=1&offset=1");
            pagedResult.setNextLink(baseApi + "/families/family2/operations?limit=1&offset=3");
            pagedResult.setLastLink(baseApi + "/families/family2/operations?limit=1&offset=3");
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return pagedResult;
    }

    public static ResourcesPagedResult mockFamiliesPagedResult(String baseApi, String limit, String offset) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_FAMILIES);
        pagedResult.setTotal(BigInteger.valueOf(5));
        if (limit == null && (offset == null || "0".equals(offset))) {
            pagedResult.getItems().add(mockFamily1Resource(baseApi));
            pagedResult.getItems().add(mockFamily2Resource(baseApi));
            pagedResult.getItems().add(mockFamily3Resource(baseApi));
            pagedResult.getItems().add(mockFamily4Resource(baseApi));
            pagedResult.getItems().add(mockFamily15Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(25));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            pagedResult.getItems().add(mockFamily1Resource(baseApi));
            pagedResult.getItems().add(mockFamily2Resource(baseApi));
            pagedResult.getItems().add(mockFamily3Resource(baseApi));
            pagedResult.getItems().add(mockFamily4Resource(baseApi));
            pagedResult.getItems().add(mockFamily15Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(1000));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pagedResult.getItems().add(mockFamily1Resource(baseApi));
            pagedResult.getItems().add(mockFamily2Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(baseApi + "/families?limit=2&offset=2");
            pagedResult.setLastLink(baseApi + "/families?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pagedResult.getItems().add(mockFamily3Resource(baseApi));
            pagedResult.getItems().add(mockFamily4Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/families?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/families?limit=2&offset=0");
            pagedResult.setNextLink(baseApi + "/families?limit=2&offset=4");
            pagedResult.setLastLink(baseApi + "/families?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            pagedResult.getItems().add(mockFamily15Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/families?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/families?limit=2&offset=2");
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/families?limit=2&offset=0");
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return pagedResult;
    }

    public static ResourcesPagedResult mockFamiliesPagedResult(String baseApi, String limit, String offset, String query) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_FAMILIES);

        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_FAMILY_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            pagedResult.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                pagedResult.getItems().add(mockFamily1Resource(baseApi));
                pagedResult.getItems().add(mockFamily15Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(0));
                pagedResult.setLimit(BigInteger.valueOf(25));
                pagedResult.setFirstLink(null);
                pagedResult.setPreviousLink(null);
                pagedResult.setNextLink(null);
                pagedResult.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                pagedResult.getItems().add(mockFamily1Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                pagedResult.setFirstLink(null);
                pagedResult.setPreviousLink(null);
                pagedResult.setNextLink(baseApi + "/families?query=" + query + "&limit=1&offset=1");
                pagedResult.setLastLink(baseApi + "/families?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                pagedResult.getItems().add(mockFamily15Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                pagedResult.setFirstLink(baseApi + "/families?query=" + query + "&limit=1&offset=0");
                pagedResult.setPreviousLink(baseApi + "/families?query=" + query + "&limit=1&offset=0");
                pagedResult.setNextLink(null);
                pagedResult.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return pagedResult;
    }

    public static ResourcesNoPagedResult mockFamiliesNoPagedResultByOperation1(String baseApi) {
        ResourcesNoPagedResult familiesResult = new ResourcesNoPagedResult();
        familiesResult.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesResult.setTotal(BigInteger.valueOf(2));
        familiesResult.getItems().add(mockFamily1Resource(baseApi));
        familiesResult.getItems().add(mockFamily2Resource(baseApi));
        return familiesResult;
    }

    public static ResourcesPagedResult mockInstancesPagedResultByOperation1(String baseApi, String limit, String offset) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_INSTANCES);
        pagedResult.setTotal(BigInteger.valueOf(5));
        String operation = StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1;
        if (limit == null && (offset == null || "0".equals(offset))) {
            pagedResult.getItems().add(mockInstance1Resource(baseApi));
            pagedResult.getItems().add(mockInstance2Resource(baseApi));
            pagedResult.getItems().add(mockInstance3Resource(baseApi));
            pagedResult.getItems().add(mockInstance4Resource(baseApi));
            pagedResult.getItems().add(mockInstance15Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(25));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            pagedResult.getItems().add(mockInstance1Resource(baseApi));
            pagedResult.getItems().add(mockInstance2Resource(baseApi));
            pagedResult.getItems().add(mockInstance3Resource(baseApi));
            pagedResult.getItems().add(mockInstance4Resource(baseApi));
            pagedResult.getItems().add(mockInstance15Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(0));
            pagedResult.setLimit(BigInteger.valueOf(1000));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pagedResult.getItems().add(mockInstance1Resource(baseApi));
            pagedResult.getItems().add(mockInstance2Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(null);
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=2");
            pagedResult.setLastLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pagedResult.getItems().add(mockInstance3Resource(baseApi));
            pagedResult.getItems().add(mockInstance4Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            pagedResult.setNextLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
            pagedResult.setLastLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            pagedResult.getItems().add(mockInstance15Resource(baseApi));
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            pagedResult.setPreviousLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=2");
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            pagedResult.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            pagedResult.setPreviousLink(null);
            pagedResult.setNextLink(null);
            pagedResult.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return pagedResult;
    }

    public static ResourcesPagedResult mockInstancesPagedResultByOperation1(String baseApi, String limit, String offset, String query) {
        ResourcesPagedResult pagedResult = new ResourcesPagedResult();
        pagedResult.setKind(RestInternalConstants.KIND_INSTANCES);
        String operation = StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1;

        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_INSTANCE_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            pagedResult.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                pagedResult.getItems().add(mockInstance1Resource(baseApi));
                pagedResult.getItems().add(mockInstance15Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(0));
                pagedResult.setLimit(BigInteger.valueOf(25));
                pagedResult.setFirstLink(null);
                pagedResult.setPreviousLink(null);
                pagedResult.setNextLink(null);
                pagedResult.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                pagedResult.getItems().add(mockInstance1Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                pagedResult.setFirstLink(null);
                pagedResult.setPreviousLink(null);
                pagedResult.setNextLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=1");
                pagedResult.setLastLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                pagedResult.getItems().add(mockInstance15Resource(baseApi));
                pagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                pagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                pagedResult.setFirstLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=0");
                pagedResult.setPreviousLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=0");
                pagedResult.setNextLink(null);
                pagedResult.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return pagedResult;
    }

    public static SimpleItemsNoPagedResult mockSurveyTypesSimpleItemsNoPagedResult() {
        SimpleItemsNoPagedResult simpleItemsNoPagedResult = new SimpleItemsNoPagedResult();
        simpleItemsNoPagedResult.setKind(RestInternalConstants.KIND_SURVEY_TYPES);
        simpleItemsNoPagedResult.setTotal(BigInteger.valueOf(2));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("surveyType1"));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("surveyType2"));
        return simpleItemsNoPagedResult;
    }

    public static SimpleItemsNoPagedResult mockOfficialityTypesSimpleItemsNoPagedResult() {
        SimpleItemsNoPagedResult simpleItemsNoPagedResult = new SimpleItemsNoPagedResult();
        simpleItemsNoPagedResult.setKind(RestInternalConstants.KIND_OFFICIALITY_TYPES);
        simpleItemsNoPagedResult.setTotal(BigInteger.valueOf(3));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("officialityType1"));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("officialityType2"));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("officialityType3"));
        return simpleItemsNoPagedResult;
    }

    public static SimpleItemsNoPagedResult mockInstanceTypesSimpleItemsNoPagedResult() {
        SimpleItemsNoPagedResult simpleItemsNoPagedResult = new SimpleItemsNoPagedResult();
        simpleItemsNoPagedResult.setKind(RestInternalConstants.KIND_INSTANCE_TYPES);
        simpleItemsNoPagedResult.setTotal(BigInteger.valueOf(2));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("instanceType1"));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("instanceType2"));
        return simpleItemsNoPagedResult;
    }

    public static SimpleItemsNoPagedResult mockSurveySourcesSimpleItemsNoPagedResult() {
        SimpleItemsNoPagedResult simpleItemsNoPagedResult = new SimpleItemsNoPagedResult();
        simpleItemsNoPagedResult.setKind(RestInternalConstants.KIND_SURVEY_SOURCES);
        simpleItemsNoPagedResult.setTotal(BigInteger.valueOf(2));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("surveySource1"));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("surveySource2"));
        return simpleItemsNoPagedResult;
    }

    public static SimpleItemsNoPagedResult mockCollMethodsSimpleItemsNoPagedResult() {
        SimpleItemsNoPagedResult simpleItemsNoPagedResult = new SimpleItemsNoPagedResult();
        simpleItemsNoPagedResult.setKind(RestInternalConstants.KIND_COLL_METHODS);
        simpleItemsNoPagedResult.setTotal(BigInteger.valueOf(2));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("collMethod1"));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("collMethod2"));
        return simpleItemsNoPagedResult;
    }

    public static SimpleItemsNoPagedResult mockCostsSimpleItemsNoPagedResult() {
        SimpleItemsNoPagedResult simpleItemsNoPagedResult = new SimpleItemsNoPagedResult();
        simpleItemsNoPagedResult.setKind(RestInternalConstants.KIND_COSTS);
        simpleItemsNoPagedResult.setTotal(BigInteger.valueOf(2));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("cost1"));
        simpleItemsNoPagedResult.getItems().add(MetamacRestMocks.mockSimpleItem("cost2"));
        return simpleItemsNoPagedResult;
    }

    public static Configuration mockExternalApiCommonMetadataRetrieveConfiguration1ById() {
        Configuration configuration = new Configuration();
        configuration.setContact(MetamacRestMocks.mockResource("contact1", "urn:contact1", "AGENCY", "http://srm-api/contacts/contact1"));
        configuration.setLegalActs(mockInternationalString("legalActs", "1"));
        configuration.setDataSharing(mockInternationalString("dataSharing", "1"));
        configuration.setConfPolicy(mockInternationalString("confidentialityPolicy", "1"));
        configuration.setConfDataTreatment(mockInternationalString("confidentialityDataTreatment", "1"));
        return configuration;
    }

    private static Resource mockOperationResource(String subId, String baseApi) {
        String operationId = "operation" + subId;
        return MetamacRestMocks.mockResource(operationId, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operationId, RestInternalConstants.KIND_OPERATION, baseApi
                + "/operations/" + operationId);
    }

    private static Resource mockFamilyResource(String subId, String baseApi) {
        String familyId = "family" + subId;
        return MetamacRestMocks.mockResource(familyId, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Family=" + familyId, RestInternalConstants.KIND_FAMILY, baseApi + "/families/"
                + familyId);
    }

    private static Resource mockInstanceResource(String operationId, String subId, String baseApi) {
        String instanceId = "instance" + subId;
        return MetamacRestMocks.mockResource(instanceId, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Instance=" + operationId + "." + instanceId,
                RestInternalConstants.KIND_INSTANCE, baseApi + "/operations/" + operationId + "/instances/" + instanceId);
    }

    private static Operation mockOperation(String baseApi, String subId, ProcStatus procStatus) {

        Operation operation = new Operation();
        operation.setId("operation" + subId);
        operation.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operation.getId());
        operation.setKind(RestInternalConstants.KIND_OPERATION);
        operation.setSelfLink(baseApi + "/operations/operation" + subId);
        operation.setTitle(mockInternationalString("operation", subId));
        operation.setAcronym(mockInternationalString("acronym", subId));
        operation.getFamilies().add(mockFamilyResource("1", baseApi));
        operation.getFamilies().add(mockFamilyResource("2", baseApi));
        operation.setSubjectArea(mockResourceFromExternalItemSrm("subjectArea1", "subjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea1", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea22", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea333", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.setObjective(mockInternationalString("objetive", subId));
        operation.setDescription(mockInternationalString("description", subId));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "1", baseApi));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "22", baseApi));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "333", baseApi));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "4444", baseApi));
        operation.setSurveyType(MetamacRestMocks.mockSimpleItem("surveyIdentifier"));
        operation.setOfficialityType(MetamacRestMocks.mockSimpleItem("officialityType"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducers().add(mockResourceFromExternalItemSrm("producer1", "producers", TypeExternalArtefactsEnum.AGENCY));
        operation.getProducers().add(mockResourceFromExternalItemSrm("producer22", "producers", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible1", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible22", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible333", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalContributors().add(mockResourceFromExternalItemSrm("regionalContributor1", "regionalContributors", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalContributors().add(mockResourceFromExternalItemSrm("regionalContributor22", "regionalContributors", TypeExternalArtefactsEnum.AGENCY));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(Status.DESIGN);
        operation.setProcStatus(procStatus);
        operation.getPublishers().add(mockResourceFromExternalItemSrm("publisher1", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.getPublishers().add(mockResourceFromExternalItemSrm("publisher22", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.getPublishers().add(mockResourceFromExternalItemSrm("publisher333", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.setRelPolUsAc(mockInternationalString("relPolUsAc", subId));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency1", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency22", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency333", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency4444", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.setCurrentInternalInstance(mockInstanceResource(operation.getId(), "22", baseApi));
        operation.setCurrentInstance(mockInstanceResource(operation.getId(), "333", baseApi));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(mockInternationalString("revPolicy", subId));
        operation.setRevPractice(mockInternationalString("revPractice", subId));
        operation.setContact(mockResourceFromExternalItem("contact1", "http://srm-api", "contacts", TypeExternalArtefactsEnum.AGENCY));
        operation.setLegalActs(mockInternationalString("legalActs", "1"));
        operation.setDataSharing(mockInternationalString("dataSharing", "1"));
        operation.setConfidentialityPolicy(mockInternationalString("confidentialityPolicy", "1"));
        operation.setConfidentialityDataTreatment(mockInternationalString("confidentialityDataTreatment", "1"));
        operation.setComment(mockInternationalString("comment", subId));
        operation.setNotes(mockInternationalString("notes", subId));
        operation.setParent(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_OPERATIONS, baseApi + "/operations"));
        operation.getChildren().add(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_FAMILIES, baseApi + "/operations/operation" + subId + "/families"));
        operation.getChildren().add(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_INSTANCES, baseApi + "/operations/operation" + subId + "/instances"));
        return operation;
    }

    private static Family mockFamily(String baseApi, String subId, ProcStatus procStatus) {

        Family family = new Family();
        family.setId("family" + subId);
        family.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Family=" + family.getId());
        family.setKind(RestInternalConstants.KIND_FAMILY);
        family.setSelfLink(baseApi + "/families/family" + subId);
        family.setTitle(mockInternationalString("family", subId));
        family.setAcronym(mockInternationalString("acronym", subId));
        family.setDescription(mockInternationalString("description", subId));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        family.setProcStatus(procStatus);
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        family.setParent(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_FAMILIES, baseApi + "/families"));
        family.getChildren().add(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_OPERATIONS, baseApi + "/families/family" + subId + "/operations"));
        return family;
    }

    private static Instance mockInstance(String baseApi, String subId, String operation, ProcStatus procStatus) {

        Instance instance = new Instance();
        instance.setId("instance" + subId);
        instance.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Instance=" + operation + "." + instance.getId());
        instance.setKind(RestInternalConstants.KIND_INSTANCE);
        instance.setSelfLink(baseApi + "/operations/" + operation + "/instances/instance" + subId);
        instance.setTitle(mockInternationalString("instance", subId));
        instance.setAcronym(mockInternationalString("acronym", subId));
        instance.setSurvey(MetamacRestMocks.mockResource(operation, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operation, RestInternalConstants.KIND_OPERATION,
                baseApi + "/operations/" + operation));
        instance.setPredecessor(mockInstanceResource(operation, "333", baseApi));
        instance.setSuccessor(mockInstanceResource(operation, "22", baseApi));
        instance.setDataDescription(mockInternationalString("dataDescription", subId));
        instance.setStatisticalPopulation(mockInternationalString("statisticalPopulation", subId));
        instance.getStatisticalUnits().add(mockResourceFromExternalItemSrm("statisticalUnit1", "statisticalUnits", TypeExternalArtefactsEnum.DATASTRUCTURE));
        instance.getStatisticalUnits().add(mockResourceFromExternalItemSrm("statisticalUnit22", "statisticalUnits", TypeExternalArtefactsEnum.DATASTRUCTURE));
        instance.setGeographicGranularity(mockResourceFromExternalItemSrm("geographicGranularity", "geographicGranularities", TypeExternalArtefactsEnum.CODELIST));
        instance.setGeographicComparability(mockInternationalString("geographicComparability", subId));
        instance.setTemporalGranularity(mockResourceFromExternalItemSrm("temporalGranularity", "temporalGranularities", TypeExternalArtefactsEnum.CODELIST));
        instance.setTemporalComparability(mockInternationalString("temporalComparability", subId));
        instance.setBasePeriod("2012");
        instance.getUnitMeasures().add(mockResourceFromExternalItemSrm("unitMeasure1", "unitMeasures", TypeExternalArtefactsEnum.CONCEPT));
        instance.setStatConcDef(mockInternationalString("statConcDef", subId));
        instance.getStatConcDefLists().add(mockResourceFromExternalItemSrm("statConcDefList1", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.getStatConcDefLists().add(mockResourceFromExternalItemSrm("statConcDefList22", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.getStatConcDefLists().add(mockResourceFromExternalItemSrm("statConcDefList333", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.setClassSystem(mockInternationalString("classSystem", subId));
        instance.getClassSystemLists().add(mockResourceFromExternalItemSrm("classSystemList1", "classSystemLists", TypeExternalArtefactsEnum.CODELIST));
        instance.getClassSystemLists().add(mockResourceFromExternalItemSrm("classSystemList22", "classSystemLists", TypeExternalArtefactsEnum.CODELIST));
        instance.setInstanceType(MetamacRestMocks.mockSimpleItem("instanceType1"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        instance.setProcStatus(procStatus);
        instance.setDocMethod(mockInternationalString("docMethod", subId));
        instance.setSurveySource(MetamacRestMocks.mockSimpleItem("surveySource1"));
        instance.setCollMethod(MetamacRestMocks.mockSimpleItem("collMethod1"));
        instance.getInformationSuppliers().add(mockResourceFromExternalItemSrm("informationSupplier1", "informationSuppliers", TypeExternalArtefactsEnum.COMMON_METADATA));
        instance.getFreqColls().add(mockResourceFromExternalItemSrm("freqColl1", "freqColls", TypeExternalArtefactsEnum.CATEGORY_SCHEME));
        instance.getFreqColls().add(mockResourceFromExternalItemSrm("freqColl22", "freqColls", TypeExternalArtefactsEnum.CATEGORY_SCHEME));
        instance.setDataValidation(mockInternationalString("dataValidation", subId));
        instance.setDataCompilation(mockInternationalString("dataCompilation", subId));
        instance.setAdjustment(mockInternationalString("adjustment", subId));
        instance.setCostBurden(mockInternationalString("costBurden", subId));
        instance.getCosts().add(MetamacRestMocks.mockSimpleItem("cost1"));
        instance.getCosts().add(MetamacRestMocks.mockSimpleItem("cost22"));
        instance.getCosts().add(MetamacRestMocks.mockSimpleItem("cost333"));
        instance.getCosts().add(MetamacRestMocks.mockSimpleItem("cost4444"));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        instance.setQualityDoc(mockInternationalString("qualityDoc", subId));
        instance.setQualityAssure(mockInternationalString("qualityAssure", subId));
        instance.setQualityAssmnt(mockInternationalString("qualityAssmnt", subId));
        instance.setUserNeeds(mockInternationalString("userNeeds", subId));
        instance.setUserSat(mockInternationalString("userSat", subId));
        instance.setCompleteness(mockInternationalString("completeness", subId));
        instance.setTimeliness(mockInternationalString("timeliness", subId));
        instance.setPunctuality(mockInternationalString("punctuality", subId));
        instance.setAccuracyOverall(mockInternationalString("accuracyOverall", subId));
        instance.setSamplingErr(mockInternationalString("samplingErr", subId));
        instance.setNonsamplingErr(mockInternationalString("nonsamplingErr", subId));
        instance.setCoherXDom(mockInternationalString("coherXDom", subId));
        instance.setCoherInternal(mockInternationalString("coherInternal", subId));
        instance.setComment(mockInternationalString("comment", subId));
        instance.setNotes(mockInternationalString("notes", subId));
        instance.setParent(MetamacRestMocks.mockResource(operation, "urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operation, RestInternalConstants.KIND_OPERATION,
                baseApi + "/operations/" + operation));
        instance.getChildren(); // no children
        return instance;
    }

    private static Resource mockOperation1Resource(String baseApi) {
        return mockOperationResource("1", baseApi);
    }

    private static Resource mockOperation2Resource(String baseApi) {
        return mockOperationResource("2", baseApi);
    }

    private static Resource mockOperation3Resource(String baseApi) {
        return mockOperationResource("3", baseApi);
    }

    private static Resource mockOperation4Resource(String baseApi) {
        return mockOperationResource("4", baseApi);
    }

    private static Resource mockOperation5Resource(String baseApi) {
        return mockOperationResource("5", baseApi);
    }

    private static Resource mockOperation6Resource(String baseApi) {
        return mockOperationResource("6", baseApi);
    }

    private static Resource mockOperation7Resource(String baseApi) {
        return mockOperationResource("7", baseApi);
    }

    private static Resource mockOperation8Resource(String baseApi) {
        return mockOperationResource("8", baseApi);
    }

    private static Resource mockOperation9Resource(String baseApi) {
        return mockOperationResource("9", baseApi);
    }

    private static Resource mockOperation10Resource(String baseApi) {
        return mockOperationResource("10", baseApi);
    }

    private static Resource mockFamily1Resource(String baseApi) {
        return mockFamilyResource("1", baseApi);
    }

    private static Resource mockFamily2Resource(String baseApi) {
        return mockFamilyResource("2", baseApi);
    }

    private static Resource mockFamily3Resource(String baseApi) {
        return mockFamilyResource("3", baseApi);
    }

    private static Resource mockFamily4Resource(String baseApi) {
        return mockFamilyResource("4", baseApi);
    }

    private static Resource mockFamily15Resource(String baseApi) {
        return mockFamilyResource("15", baseApi);
    }

    private static Resource mockInstance1Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1, "1", baseApi);
    }

    private static Resource mockInstance2Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1, "2", baseApi);
    }

    private static Resource mockInstance3Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1, "3", baseApi);
    }

    private static Resource mockInstance4Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1, "4", baseApi);
    }

    private static Resource mockInstance15Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1, "15", baseApi);
    }

    private static InternationalString mockInternationalString(String metadata, String subsubTitle) {
        String subTitle = subsubTitle != null ? metadata + subsubTitle : metadata;
        return MetamacRestMocks.mockInternationalString("es", subTitle + " en Español", "en", subTitle + " in English");
    }

    private static Resource mockResourceFromExternalItemSrm(String id, String apiSubpath, TypeExternalArtefactsEnum kind) {
        String endpointApi = "http://localhost:8080/metamac-srm-web"; // not read property from properties file to check explicity
        return mockResourceFromExternalItem(id, endpointApi, apiSubpath, kind);
    }

    private static Resource mockResourceFromExternalItem(String id, String endpointApi, String apiSubpath, TypeExternalArtefactsEnum kind) {
        String urn = "urn:" + id;
        String selfLink = endpointApi + "/" + apiSubpath + "/" + id;
        return MetamacRestMocks.mockResource(id, urn, kind.name(), selfLink);
    }
}