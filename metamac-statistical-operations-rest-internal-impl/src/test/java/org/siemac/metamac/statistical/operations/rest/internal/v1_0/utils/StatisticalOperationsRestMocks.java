package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import static org.junit.Assert.fail;

import java.math.BigInteger;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.ProcStatus;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.service.StatisticalOperationsRestFacadeV10Test;

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
        String querySupported1 = StatisticalOperationsRestFacadeV10Test.QUERY_OPERATION_CODE_LIKE_1;
        String querySupported2 = StatisticalOperationsRestFacadeV10Test.QUERY_OPERATION_CODE_LIKE_1_AND_INDICATORS_SYSTEM;
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
            fail("Query not supported = " + offset);
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

        String querySupported1 = StatisticalOperationsRestFacadeV10Test.QUERY_FAMILY_CODE_LIKE_1;
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
        String operation = StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1;
        if (limit == null && (offset == null || "0".equals(offset))) {
            pagedResult.getItems().add(mockInstance1Resource(baseApi));
            pagedResult.getItems().add(mockInstance2Resource(baseApi));
            pagedResult.getItems().add(mockInstance3Resource(baseApi));
            pagedResult.getItems().add(mockInstance4Resource(baseApi));
            pagedResult.getItems().add(mockInstance5Resource(baseApi));
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
            pagedResult.getItems().add(mockInstance5Resource(baseApi));
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
            pagedResult.getItems().add(mockInstance5Resource(baseApi));
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

    private static Resource mockOperationResource(String subId, String baseApi) {
        return MetamacRestMocks.mockResource("operation" + subId, RestInternalConstants.KIND_OPERATION, baseApi + "/operations/operation" + subId);
    }

    private static Resource mockFamilyResource(String subId, String baseApi) {
        return MetamacRestMocks.mockResource("family" + subId, RestInternalConstants.KIND_FAMILY, baseApi + "/families/family" + subId);
    }

    private static Resource mockInstanceResource(String operationId, String subId, String baseApi) {
        return MetamacRestMocks.mockResource("instance" + subId, RestInternalConstants.KIND_INSTANCE, baseApi + "/operations/" + operationId + "/instances/instance" + subId);
    }

    private static Operation mockOperation(String baseApi, String subCode, ProcStatus procStatus) {

        Operation operation = new Operation();
        operation.setId("operation" + subCode);
        operation.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Operation=" + operation.getId());
        operation.setKind(RestInternalConstants.KIND_OPERATION);
        operation.setSelfLink(baseApi + "/operations/operation" + subCode);
        operation.setTitle(mockInternationalString("operation", subCode));
        operation.setAcronym(mockInternationalString("acronym", subCode));
        operation.getFamilies().add(mockFamilyResource("1", baseApi));
        operation.getFamilies().add(mockFamilyResource("2", baseApi));
        operation.setSubjectArea(MetamacRestMocks.mockResource("subjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), "http://subjectArea1"));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockResource("secundarySubjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), "http://secundarySubjectArea1"));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockResource("secundarySubjectArea22", TypeExternalArtefactsEnum.CATEGORY.name(), "http://secundarySubjectArea22"));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockResource("secundarySubjectArea333", TypeExternalArtefactsEnum.CATEGORY.name(), "http://secundarySubjectArea333"));
        operation.setObjective(mockInternationalString("objetive", subCode));
        operation.setDescription(mockInternationalString("description", subCode));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "1", baseApi));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "22", baseApi));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "333", baseApi));
        operation.getInstances().add(mockInstanceResource(operation.getId(), "4444", baseApi));
        operation.setSurveyType(MetamacRestMocks.mockResource("surveyIdentifier", null, null));
        operation.setOfficialityType(MetamacRestMocks.mockResource("officialityType", null, null));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducers().add(MetamacRestMocks.mockResource("producer1", TypeExternalArtefactsEnum.AGENCY.name(), "http://producer1"));
        operation.getProducers().add(MetamacRestMocks.mockResource("producer22", TypeExternalArtefactsEnum.AGENCY.name(), "http://producer22"));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockResource("regionalResponsible1", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalResponsible1"));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockResource("regionalResponsible22", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalResponsible22"));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockResource("regionalResponsible333", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalResponsible333"));
        operation.getRegionalContributors().add(MetamacRestMocks.mockResource("regionalContributor1", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalContributor1"));
        operation.getRegionalContributors().add(MetamacRestMocks.mockResource("regionalContributor22", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalContributor22"));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN.name());
        operation.setProcStatus(procStatus);
        operation.getPublishers().add(MetamacRestMocks.mockResource("publisher1", TypeExternalArtefactsEnum.AGENCY.name(), "http://publisher1"));
        operation.getPublishers().add(MetamacRestMocks.mockResource("publisher22", TypeExternalArtefactsEnum.AGENCY.name(), "http://publisher22"));
        operation.getPublishers().add(MetamacRestMocks.mockResource("publisher333", TypeExternalArtefactsEnum.AGENCY.name(), "http://publisher333"));
        operation.setRelPolUsAc(mockInternationalString("relPolUsAc", subCode));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockResource("updateFrequency1", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency1"));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockResource("updateFrequency22", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency22"));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockResource("updateFrequency333", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency333"));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockResource("updateFrequency4444", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency4444"));
        operation.setCurrentInternalInstance(mockInstanceResource(operation.getId(), "22", baseApi));
        operation.setCurrentInstance(mockInstanceResource(operation.getId(), "333", baseApi));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(mockInternationalString("revPolicy", subCode));
        operation.setRevPractice(mockInternationalString("revPractice", subCode));
        // TODO CONTACTS, LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        operation.setComment(mockInternationalString("comment", subCode));
        operation.setNotes(mockInternationalString("notes", subCode));
        operation.setParent(MetamacRestMocks.mockResourceWithoutTitle(null, RestInternalConstants.KIND_OPERATIONS, baseApi + "/operations"));
        operation.getchildren().add(MetamacRestMocks.mockResourceWithoutTitle(null, RestInternalConstants.KIND_FAMILIES, baseApi + "/operations/operation" + subCode + "/families"));
        operation.getchildren().add(MetamacRestMocks.mockResourceWithoutTitle(null, RestInternalConstants.KIND_INSTANCES, baseApi + "/operations/operation" + subCode + "/instances"));
        return operation;
    }

    private static Family mockFamily(String baseApi, String subCode, ProcStatus procStatus) {

        Family family = new Family();
        family.setId("family" + subCode);
        family.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Family=" + family.getId());
        family.setKind(RestInternalConstants.KIND_FAMILY);
        family.setSelfLink(baseApi + "/families/family" + subCode);
        family.setTitle(mockInternationalString("family", subCode));
        family.setAcronym(mockInternationalString("acronym", subCode));
        family.setDescription(mockInternationalString("description", subCode));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        family.setProcStatus(procStatus);
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        family.setParent(MetamacRestMocks.mockResourceWithoutTitle(null, RestInternalConstants.KIND_FAMILIES, baseApi + "/families"));
        family.getchildren().add(MetamacRestMocks.mockResourceWithoutTitle(null, RestInternalConstants.KIND_OPERATIONS, baseApi + "/families/family" + subCode + "/operations"));
        return family;
    }

    private static Instance mockInstance(String baseApi, String subCode, String operation, ProcStatus procStatus) {

        Instance instance = new Instance();
        instance.setId("instance" + subCode);
        instance.setUrn("urn:siemac:org.siemac.metamac.infomodel.statisticaloperations.Instance=" + operation + "." + instance.getId());
        instance.setKind(RestInternalConstants.KIND_INSTANCE);
        instance.setSelfLink(baseApi + "/operations/" + operation + "/instances/instance" + subCode);
        instance.setTitle(mockInternationalString("instance", subCode));
        instance.setAcronym(mockInternationalString("acronym", subCode));
        instance.setSurvey(MetamacRestMocks.mockResource(operation, RestInternalConstants.KIND_OPERATION, baseApi + "/operations/" + operation));
        instance.setPredecessor(mockInstanceResource(operation, "333", baseApi));
        instance.setSuccessor(mockInstanceResource(operation, "22", baseApi));
        instance.setDataDescription(mockInternationalString("dataDescription", subCode));
        instance.setStatisticalPopulation(mockInternationalString("statisticalPopulation", subCode));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockResource("statisticalUnit1", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), "http://statisticalUnit1"));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockResource("statisticalUnit22", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), "http://statisticalUnit22"));
        instance.setGeographicGranularity(MetamacRestMocks.mockResource("geographicGranularity", TypeExternalArtefactsEnum.CODELIST.name(), "http://geographicGranularity"));
        instance.setGeographicComparability(mockInternationalString("geographicComparability", subCode));
        instance.setTemporalGranularity(MetamacRestMocks.mockResource("temporalGranularity", TypeExternalArtefactsEnum.CODELIST.name(), "http://temporalGranularity"));
        instance.setTemporalComparability(mockInternationalString("temporalComparability", subCode));
        instance.setBasePeriod("2012");
        instance.getUnitMeasures().add(MetamacRestMocks.mockResource("unitMeasure1", TypeExternalArtefactsEnum.CONCEPT.name(), "http://unitMeasure1"));
        instance.setStatConcDef(mockInternationalString("statConcDef", subCode));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList1"));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockResource("statConcDefList22", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList22"));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockResource("statConcDefList333", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList333"));
        instance.setClassSystem(mockInternationalString("classSystem", subCode));
        instance.getClassSystemLists().add(MetamacRestMocks.mockResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList1"));
        instance.getClassSystemLists().add(MetamacRestMocks.mockResource("statConcDefList22", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList22"));
        instance.setInstanceType(MetamacRestMocks.mockResource("instanceType1", null, null));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        instance.setProcStatus(procStatus);
        instance.setDocMethod(mockInternationalString("docMethod", subCode));
        instance.setSurveySource(MetamacRestMocks.mockResource("surveySource1", null, null));
        instance.setCollMethod(MetamacRestMocks.mockResource("collMethod1", null, null));
        instance.getInformationSuppliers().add(MetamacRestMocks.mockResource("informationSupplier1", TypeExternalArtefactsEnum.COMMON_METADATA.name(), "http://informationSupplier1"));
        instance.getFreqColls().add(MetamacRestMocks.mockResource("freqColl1", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), "http://freqColl1"));
        instance.getFreqColls().add(MetamacRestMocks.mockResource("freqColl22", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), "http://freqColl22"));
        instance.setDataValidation(mockInternationalString("dataValidation", subCode));
        instance.setDataCompilation(mockInternationalString("dataCompilation", subCode));
        instance.setAdjustment(mockInternationalString("adjustment", subCode));
        instance.setCostBurden(mockInternationalString("costBurden", subCode));
        instance.getCosts().add(MetamacRestMocks.mockResource("cost1", null, null));
        instance.getCosts().add(MetamacRestMocks.mockResource("cost22", null, null));
        instance.getCosts().add(MetamacRestMocks.mockResource("cost333", null, null));
        instance.getCosts().add(MetamacRestMocks.mockResource("cost4444", null, null));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        instance.setQualityDoc(mockInternationalString("qualityDoc", subCode));
        instance.setQualityAssure(mockInternationalString("qualityAssure", subCode));
        instance.setQualityAssmnt(mockInternationalString("qualityAssmnt", subCode));
        instance.setUserNeeds(mockInternationalString("userNeeds", subCode));
        instance.setUserSat(mockInternationalString("userSat", subCode));
        instance.setCompleteness(mockInternationalString("completeness", subCode));
        instance.setTimeliness(mockInternationalString("timeliness", subCode));
        instance.setPunctuality(mockInternationalString("punctuality", subCode));
        instance.setAccuracyOverall(mockInternationalString("accuracyOverall", subCode));
        instance.setSamplingErr(mockInternationalString("samplingErr", subCode));
        instance.setNonsamplingErr(mockInternationalString("nonsamplingErr", subCode));
        instance.setCoherXDom(mockInternationalString("coherXDom", subCode));
        instance.setCoherInternal(mockInternationalString("coherInternal", subCode));
        instance.setComment(mockInternationalString("comment", subCode));
        instance.setNotes(mockInternationalString("notes", subCode));
        instance.setParent(MetamacRestMocks.mockResourceWithoutTitle(null, RestInternalConstants.KIND_OPERATION, baseApi + "/operations/" + operation));
        instance.getchildren(); // no children
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
        return mockInstanceResource(StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1, "1", baseApi);
    }

    private static Resource mockInstance2Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1, "2", baseApi);
    }

    private static Resource mockInstance3Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1, "3", baseApi);
    }

    private static Resource mockInstance4Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1, "4", baseApi);
    }

    private static Resource mockInstance5Resource(String baseApi) {
        return mockInstanceResource(StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1, "5", baseApi);
    }

    private static InternationalString mockInternationalString(String metadata, String subCode) {
        String subTitle = subCode != null ? metadata + subCode : metadata;
        return MetamacRestMocks.mockInternationalString("es", subTitle + " en Español", "en", subTitle + " in English");
    }
}