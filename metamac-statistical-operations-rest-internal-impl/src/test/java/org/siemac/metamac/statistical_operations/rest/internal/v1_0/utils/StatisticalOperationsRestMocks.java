package org.siemac.metamac.statistical_operations.rest.internal.v1_0.utils;

import static org.junit.Assert.fail; 

import java.math.BigInteger;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.statistical_operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Children;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.ClassSystems;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.CollMethods;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Costs;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Families;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.FreqColls;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.InformationSuppliers;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Instances;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.ProcStatus;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Producers;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Publishers;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.RegionalContributors;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.RegionalResponsibles;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.SecondarySubjectAreas;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.StatConcDefs;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.StatisticalUnits;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Status;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.SurveySources;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.SurveyTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.UnitMeasures;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.UpdateFrequencies;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.mapper.Do2RestInternalMapperV10Impl;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.service.StatisticalOperationsRestInternalFacadeV10Test;

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

    public static Operations mockOperations(String baseApi, String limit, String offset) {
        Operations operations = new Operations();
        operations.setKind(RestInternalConstants.KIND_OPERATIONS);
        operations.setTotal(BigInteger.valueOf(10));
        if (limit == null && (offset == null || "0".equals(offset))) {
            operations.getOperations().add(mockOperation1Resource(baseApi));
            operations.getOperations().add(mockOperation2Resource(baseApi));
            operations.getOperations().add(mockOperation3Resource(baseApi));
            operations.getOperations().add(mockOperation4Resource(baseApi));
            operations.getOperations().add(mockOperation5Resource(baseApi));
            operations.getOperations().add(mockOperation6Resource(baseApi));
            operations.getOperations().add(mockOperation7Resource(baseApi));
            operations.getOperations().add(mockOperation8Resource(baseApi));
            operations.getOperations().add(mockOperation9Resource(baseApi));
            operations.getOperations().add(mockOperation10Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(25));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            operations.getOperations().add(mockOperation1Resource(baseApi));
            operations.getOperations().add(mockOperation2Resource(baseApi));
            operations.getOperations().add(mockOperation3Resource(baseApi));
            operations.getOperations().add(mockOperation4Resource(baseApi));
            operations.getOperations().add(mockOperation5Resource(baseApi));
            operations.getOperations().add(mockOperation6Resource(baseApi));
            operations.getOperations().add(mockOperation7Resource(baseApi));
            operations.getOperations().add(mockOperation8Resource(baseApi));
            operations.getOperations().add(mockOperation9Resource(baseApi));
            operations.getOperations().add(mockOperation10Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(1000));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            operations.getOperations().add(mockOperation1Resource(baseApi));
            operations.getOperations().add(mockOperation2Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(baseApi + "/operations?limit=2&offset=2");
            operations.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            operations.getOperations().add(mockOperation3Resource(baseApi));
            operations.getOperations().add(mockOperation4Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/operations?limit=2&offset=0");
            operations.setNextLink(baseApi + "/operations?limit=2&offset=4");
            operations.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "8".equals(offset)) {
            operations.getOperations().add(mockOperation9Resource(baseApi));
            operations.getOperations().add(mockOperation10Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/operations?limit=2&offset=6");
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operations;
    }

    public static Operations mockOperations(String baseApi, String limit, String offset, String query) {
        Operations operations = new Operations();
        operations.setKind(RestInternalConstants.KIND_OPERATIONS);
        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1;
        String querySupported2 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM;
        if (querySupported1.equals(query)) {
            operations.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                operations.getOperations().add(mockOperation1Resource(baseApi));
                operations.getOperations().add(mockOperation10Resource(baseApi));
                operations.setOffset(BigInteger.valueOf(0));
                operations.setLimit(BigInteger.valueOf(25));
                operations.setFirstLink(null);
                operations.setPreviousLink(null);
                operations.setNextLink(null);
                operations.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                operations.getOperations().add(mockOperation1Resource(baseApi));
                operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                operations.setFirstLink(null);
                operations.setPreviousLink(null);
                operations.setNextLink(baseApi + "/operations?query=" + query + "&limit=1&offset=1");
                operations.setLastLink(baseApi + "/operations?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                operations.getOperations().add(mockOperation3Resource(baseApi));
                operations.getOperations().add(mockOperation4Resource(baseApi));
                operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                operations.setFirstLink(baseApi + "/operations?query=" + query + "&limit=1&offset=0");
                operations.setPreviousLink(baseApi + "/operations?query=" + query + "&limit=1&offset=0");
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else if (querySupported2.equals(query)) {
            operations.setTotal(BigInteger.valueOf(1));
            if (limit == null && (offset == null || "0".equals(offset))) {
                operations.getOperations().add(mockOperation1Resource(baseApi));
                operations.setOffset(BigInteger.valueOf(0));
                operations.setLimit(BigInteger.valueOf(25));
                operations.setFirstLink(null);
                operations.setPreviousLink(null);
                operations.setNextLink(null);
                operations.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return operations;
    }

    public static Operations mockOperationsByFamily1(String baseApi, String limit, String offset) {
        Operations operations = new Operations();
        operations.setKind(RestInternalConstants.KIND_OPERATIONS);
        operations.setTotal(BigInteger.valueOf(6));
        if (limit == null && (offset == null || "0".equals(offset))) {
            operations.getOperations().add(mockOperation1Resource(baseApi));
            operations.getOperations().add(mockOperation2Resource(baseApi));
            operations.getOperations().add(mockOperation3Resource(baseApi));
            operations.getOperations().add(mockOperation4Resource(baseApi));
            operations.getOperations().add(mockOperation5Resource(baseApi));
            operations.getOperations().add(mockOperation6Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(25));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            operations.getOperations().add(mockOperation1Resource(baseApi));
            operations.getOperations().add(mockOperation2Resource(baseApi));
            operations.getOperations().add(mockOperation3Resource(baseApi));
            operations.getOperations().add(mockOperation4Resource(baseApi));
            operations.getOperations().add(mockOperation5Resource(baseApi));
            operations.getOperations().add(mockOperation6Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(0));
            operations.setLimit(BigInteger.valueOf(1000));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            operations.getOperations().add(mockOperation1Resource(baseApi));
            operations.getOperations().add(mockOperation2Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(null);
            operations.setPreviousLink(null);
            operations.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            operations.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            operations.getOperations().add(mockOperation3Resource(baseApi));
            operations.getOperations().add(mockOperation4Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=4");
            operations.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            operations.getOperations().add(mockOperation5Resource(baseApi));
            operations.getOperations().add(mockOperation6Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else if ("2".equals(limit) && "7".equals(offset)) {
            // no results
            operations.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operations.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operations.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operations.setPreviousLink(null);
            operations.setNextLink(null);
            operations.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operations;
    }

    public static Operations mockOperationsByFamily2(String baseApi, String limit, String offset) {
        Operations operations = new Operations();
        operations.setKind(RestInternalConstants.KIND_OPERATIONS);
        operations.setTotal(BigInteger.valueOf(4));
        if ("1".equals(limit) && "2".equals(offset)) {
            operations.getOperations().add(mockOperation8Resource(baseApi));
            operations.setOffset(BigInteger.valueOf(2));
            operations.setLimit(BigInteger.valueOf(1));
            operations.setFirstLink(baseApi + "/families/family2/operations?limit=1&offset=0");
            operations.setPreviousLink(baseApi + "/families/family2/operations?limit=1&offset=1");
            operations.setNextLink(baseApi + "/families/family2/operations?limit=1&offset=3");
            operations.setLastLink(baseApi + "/families/family2/operations?limit=1&offset=3");
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operations;
    }

    public static Families mockFamilies(String baseApi, String limit, String offset) {
        Families families = new Families();
        families.setKind(RestInternalConstants.KIND_FAMILIES);
        families.setTotal(BigInteger.valueOf(5));
        if (limit == null && (offset == null || "0".equals(offset))) {
            families.getFamilies().add(mockFamily1Resource(baseApi));
            families.getFamilies().add(mockFamily2Resource(baseApi));
            families.getFamilies().add(mockFamily3Resource(baseApi));
            families.getFamilies().add(mockFamily4Resource(baseApi));
            families.getFamilies().add(mockFamily15Resource(baseApi));
            families.setOffset(BigInteger.valueOf(0));
            families.setLimit(BigInteger.valueOf(25));
            families.setFirstLink(null);
            families.setPreviousLink(null);
            families.setNextLink(null);
            families.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            families.getFamilies().add(mockFamily1Resource(baseApi));
            families.getFamilies().add(mockFamily2Resource(baseApi));
            families.getFamilies().add(mockFamily3Resource(baseApi));
            families.getFamilies().add(mockFamily4Resource(baseApi));
            families.getFamilies().add(mockFamily15Resource(baseApi));
            families.setOffset(BigInteger.valueOf(0));
            families.setLimit(BigInteger.valueOf(1000));
            families.setFirstLink(null);
            families.setPreviousLink(null);
            families.setNextLink(null);
            families.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            families.getFamilies().add(mockFamily1Resource(baseApi));
            families.getFamilies().add(mockFamily2Resource(baseApi));
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(null);
            families.setPreviousLink(null);
            families.setNextLink(baseApi + "/families?limit=2&offset=2");
            families.setLastLink(baseApi + "/families?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            families.getFamilies().add(mockFamily3Resource(baseApi));
            families.getFamilies().add(mockFamily4Resource(baseApi));
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(baseApi + "/families?limit=2&offset=0");
            families.setPreviousLink(baseApi + "/families?limit=2&offset=0");
            families.setNextLink(baseApi + "/families?limit=2&offset=4");
            families.setLastLink(baseApi + "/families?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            families.getFamilies().add(mockFamily15Resource(baseApi));
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(baseApi + "/families?limit=2&offset=0");
            families.setPreviousLink(baseApi + "/families?limit=2&offset=2");
            families.setNextLink(null);
            families.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            families.setFirstLink(baseApi + "/families?limit=2&offset=0");
            families.setPreviousLink(null);
            families.setNextLink(null);
            families.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return families;
    }

    public static Families mockFamilies(String baseApi, String limit, String offset, String query) {
        Families families = new Families();
        families.setKind(RestInternalConstants.KIND_FAMILIES);

        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_FAMILY_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            families.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                families.getFamilies().add(mockFamily1Resource(baseApi));
                families.getFamilies().add(mockFamily15Resource(baseApi));
                families.setOffset(BigInteger.valueOf(0));
                families.setLimit(BigInteger.valueOf(25));
                families.setFirstLink(null);
                families.setPreviousLink(null);
                families.setNextLink(null);
                families.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                families.getFamilies().add(mockFamily1Resource(baseApi));
                families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                families.setFirstLink(null);
                families.setPreviousLink(null);
                families.setNextLink(baseApi + "/families?query=" + query + "&limit=1&offset=1");
                families.setLastLink(baseApi + "/families?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                families.getFamilies().add(mockFamily15Resource(baseApi));
                families.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                families.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                families.setFirstLink(baseApi + "/families?query=" + query + "&limit=1&offset=0");
                families.setPreviousLink(baseApi + "/families?query=" + query + "&limit=1&offset=0");
                families.setNextLink(null);
                families.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return families;
    }

    public static Families mockFamiliesByOperation1(String baseApi) {
        Families families = new Families();
        families.setKind(RestInternalConstants.KIND_FAMILIES);
        families.setTotal(BigInteger.valueOf(2));
        families.getFamilies().add(mockFamily1Resource(baseApi));
        families.getFamilies().add(mockFamily2Resource(baseApi));
        return families;
    }

    public static Instances mockInstancesByOperation1(String baseApi, String limit, String offset) {
        Instances instances = new Instances();
        instances.setKind(RestInternalConstants.KIND_INSTANCES);
        instances.setTotal(BigInteger.valueOf(5));
        String operation = StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1;
        if (limit == null && (offset == null || "0".equals(offset))) {
            instances.getInstances().add(mockInstance1Resource(baseApi));
            instances.getInstances().add(mockInstance2Resource(baseApi));
            instances.getInstances().add(mockInstance3Resource(baseApi));
            instances.getInstances().add(mockInstance4Resource(baseApi));
            instances.getInstances().add(mockInstance15Resource(baseApi));
            instances.setOffset(BigInteger.valueOf(0));
            instances.setLimit(BigInteger.valueOf(25));
            instances.setFirstLink(null);
            instances.setPreviousLink(null);
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            instances.getInstances().add(mockInstance1Resource(baseApi));
            instances.getInstances().add(mockInstance2Resource(baseApi));
            instances.getInstances().add(mockInstance3Resource(baseApi));
            instances.getInstances().add(mockInstance4Resource(baseApi));
            instances.getInstances().add(mockInstance15Resource(baseApi));
            instances.setOffset(BigInteger.valueOf(0));
            instances.setLimit(BigInteger.valueOf(1000));
            instances.setFirstLink(null);
            instances.setPreviousLink(null);
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            instances.getInstances().add(mockInstance1Resource(baseApi));
            instances.getInstances().add(mockInstance2Resource(baseApi));
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(null);
            instances.setPreviousLink(null);
            instances.setNextLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=2");
            instances.setLastLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            instances.getInstances().add(mockInstance3Resource(baseApi));
            instances.getInstances().add(mockInstance4Resource(baseApi));
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setPreviousLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setNextLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
            instances.setLastLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            instances.getInstances().add(mockInstance15Resource(baseApi));
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setPreviousLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=2");
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?limit=2&offset=0");
            instances.setPreviousLink(null);
            instances.setNextLink(null);
            instances.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return instances;
    }

    public static Instances mockInstancesByOperation1(String baseApi, String limit, String offset, String query) {
        Instances instances = new Instances();
        instances.setKind(RestInternalConstants.KIND_INSTANCES);
        String operation = StatisticalOperationsRestInternalFacadeV10Test.OPERATION_1;

        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_INSTANCE_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            instances.setTotal(BigInteger.valueOf(2));
            if (limit == null && (offset == null || "0".equals(offset))) {
                instances.getInstances().add(mockInstance1Resource(baseApi));
                instances.getInstances().add(mockInstance15Resource(baseApi));
                instances.setOffset(BigInteger.valueOf(0));
                instances.setLimit(BigInteger.valueOf(25));
                instances.setFirstLink(null);
                instances.setPreviousLink(null);
                instances.setNextLink(null);
                instances.setLastLink(null);
            } else if ("1".equals(limit) && "0".equals(offset)) {
                instances.getInstances().add(mockInstance1Resource(baseApi));
                instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                instances.setFirstLink(null);
                instances.setPreviousLink(null);
                instances.setNextLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=1");
                instances.setLastLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=1");
            } else if ("1".equals(limit) && "1".equals(offset)) {
                instances.getInstances().add(mockInstance15Resource(baseApi));
                instances.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
                instances.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
                instances.setFirstLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=0");
                instances.setPreviousLink(baseApi + "/operations/" + operation + "/instances?query=" + query + "&limit=1&offset=0");
                instances.setNextLink(null);
                instances.setLastLink(null);
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return instances;
    }

    public static SurveyTypes mockSurveyTypes() {
        SurveyTypes surveyTypes = new SurveyTypes();
        surveyTypes.setKind(RestInternalConstants.KIND_SURVEY_TYPES);
        surveyTypes.setTotal(BigInteger.valueOf(2));
        surveyTypes.getSurveyTypes().add(MetamacRestMocks.mockItem("surveyType1"));
        surveyTypes.getSurveyTypes().add(MetamacRestMocks.mockItem("surveyType2"));
        return surveyTypes;
    }

    public static OfficialityTypes mockOfficialityTypes() {
        OfficialityTypes officialityTypes = new OfficialityTypes();
        officialityTypes.setKind(RestInternalConstants.KIND_OFFICIALITY_TYPES);
        officialityTypes.setTotal(BigInteger.valueOf(3));
        officialityTypes.getOfficialityTypes().add(MetamacRestMocks.mockItem("officialityType1"));
        officialityTypes.getOfficialityTypes().add(MetamacRestMocks.mockItem("officialityType2"));
        officialityTypes.getOfficialityTypes().add(MetamacRestMocks.mockItem("officialityType3"));
        return officialityTypes;
    }

    public static InstanceTypes mockInstanceTypes() {
        InstanceTypes instanceTypes = new InstanceTypes();
        instanceTypes.setKind(RestInternalConstants.KIND_INSTANCE_TYPES);
        instanceTypes.setTotal(BigInteger.valueOf(2));
        instanceTypes.getInstanceTypes().add(MetamacRestMocks.mockItem("instanceType1"));
        instanceTypes.getInstanceTypes().add(MetamacRestMocks.mockItem("instanceType2"));
        return instanceTypes;
    }

    public static SurveySources mockSurveySources() {
        SurveySources surveySources = new SurveySources();
        surveySources.setKind(RestInternalConstants.KIND_SURVEY_SOURCES);
        surveySources.setTotal(BigInteger.valueOf(2));
        surveySources.getSurveySources().add(MetamacRestMocks.mockItem("surveySource1"));
        surveySources.getSurveySources().add(MetamacRestMocks.mockItem("surveySource2"));
        return surveySources;
    }

    public static CollMethods mockCollMethods() {
        CollMethods collMethods = new CollMethods();
        collMethods.setKind(RestInternalConstants.KIND_COLL_METHODS);
        collMethods.setTotal(BigInteger.valueOf(2));
        collMethods.getCollMethods().add(MetamacRestMocks.mockItem("collMethod1"));
        collMethods.getCollMethods().add(MetamacRestMocks.mockItem("collMethod2"));
        return collMethods;
    }

    public static Costs mockCosts() {
        Costs costs = new Costs();
        costs.setKind(RestInternalConstants.KIND_COSTS);
        costs.setTotal(BigInteger.valueOf(2));
        costs.getCosts().add(MetamacRestMocks.mockItem("cost1"));
        costs.getCosts().add(MetamacRestMocks.mockItem("cost2"));
        return costs;
    }

    // TODO
    // public static Configuration mockExternalApiCommonMetadataRetrieveConfiguration1ById() {
    // Configuration configuration = new Configuration();
    // configuration.setContact(MetamacRestMocks.mockResource("contact1", "urn:contact1", "AGENCY", "http://srm-api/contacts/contact1"));
    // configuration.setLegalActs(mockInternationalString("legalActs", "1"));
    // configuration.setDataSharing(mockInternationalString("dataSharing", "1"));
    // configuration.setConfPolicy(mockInternationalString("confidentialityPolicy", "1"));
    // configuration.setConfDataTreatment(mockInternationalString("confidentialityDataTreatment", "1"));
    // return configuration;
    // }

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
        operation.setSubjectArea(mockResourceFromExternalItemSrm("subjectArea1", "subjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.setSecondarySubjectAreas(new SecondarySubjectAreas());
        operation.getSecondarySubjectAreas().setTotal(BigInteger.valueOf(3));
        operation.getSecondarySubjectAreas().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getSecondarySubjectAreas().getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea1", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea22", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().getSecondarySubjectAreas().add(mockResourceFromExternalItemSrm("secundarySubjectArea333", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.setObjective(mockInternationalString("objetive", subId));
        operation.setDescription(mockInternationalString("description", subId));
        // TODO
//        operation.getInstances().add(mockInstanceResource(operation.getId(), "1", baseApi));
//        operation.getInstances().add(mockInstanceResource(operation.getId(), "22", baseApi));
//        operation.getInstances().add(mockInstanceResource(operation.getId(), "333", baseApi));
//        operation.getInstances().add(mockInstanceResource(operation.getId(), "4444", baseApi));
        operation.setSurveyType(MetamacRestMocks.mockItem("surveyIdentifier"));
        operation.setOfficialityType(MetamacRestMocks.mockItem("officialityType"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.setProducers(new Producers());
        operation.getProducers().setTotal(BigInteger.valueOf(2));
        operation.getProducers().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getProducers().getProducers().add(mockResourceFromExternalItemSrm("producer1", "producers", TypeExternalArtefactsEnum.AGENCY));
        operation.getProducers().getProducers().add(mockResourceFromExternalItemSrm("producer22", "producers", TypeExternalArtefactsEnum.AGENCY));
        operation.setRegionalResponsibles(new RegionalResponsibles());
        operation.getRegionalResponsibles().setTotal(BigInteger.valueOf(3));
        operation.getRegionalResponsibles().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getRegionalResponsibles().getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible1", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsibles().getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible22", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsibles().getRegionalResponsibles().add(mockResourceFromExternalItemSrm("regionalResponsible333", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.setRegionalContributors(new RegionalContributors());
        operation.getRegionalContributors().setTotal(BigInteger.valueOf(2));
        operation.getRegionalContributors().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getRegionalContributors().setTotal(BigInteger.valueOf(2));
        operation.getRegionalContributors().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getRegionalContributors().getRegionalContributors().add(mockResourceFromExternalItemSrm("regionalContributor1", "regionalContributors", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalContributors().getRegionalContributors().add(mockResourceFromExternalItemSrm("regionalContributor22", "regionalContributors", TypeExternalArtefactsEnum.AGENCY));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(Status.DESIGN);
        operation.setProcStatus(procStatus);
        operation.setPublishers(new Publishers());
        operation.getPublishers().setTotal(BigInteger.valueOf(3));
        operation.getPublishers().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getPublishers().getPublishers().add(mockResourceFromExternalItemSrm("publisher1", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.getPublishers().getPublishers().add(mockResourceFromExternalItemSrm("publisher22", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.getPublishers().getPublishers().add(mockResourceFromExternalItemSrm("publisher333", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.setRelPolUsAc(mockInternationalString("relPolUsAc", subId));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.setUpdateFrequencies(new UpdateFrequencies());
        operation.getUpdateFrequencies().setTotal(BigInteger.valueOf(4));
        operation.getUpdateFrequencies().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency1", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency22", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency333", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequencies().getUpdateFrequencies().add(mockResourceFromExternalItemSrm("updateFrequency4444", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.setCurrentInternalInstance(mockInstanceResource(operation.getId(), "22", baseApi));
        operation.setCurrentInstance(mockInstanceResource(operation.getId(), "333", baseApi));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(mockInternationalString("revPolicy", subId));
        operation.setRevPractice(mockInternationalString("revPractice", subId));
        // TODO
        // operation.setContact(mockResourceFromExternalItem("contact1", "http://srm-api", "contacts", TypeExternalArtefactsEnum.AGENCY));
        // operation.setLegalActs(mockInternationalString("legalActs", "1"));
        // operation.setDataSharing(mockInternationalString("dataSharing", "1"));
        // operation.setConfidentialityPolicy(mockInternationalString("confidentialityPolicy", "1"));
        // operation.setConfidentialityDataTreatment(mockInternationalString("confidentialityDataTreatment", "1"));
        operation.setComment(mockInternationalString("comment", subId));
        operation.setNotes(mockInternationalString("notes", subId));
        operation.setParent(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_OPERATIONS, baseApi + "/operations"));
        operation.setChildren(new Children());
        operation.getChildren().setTotal(BigInteger.valueOf(2));
        operation.getChildren().getChildren().add(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_FAMILIES, baseApi + "/operations/operation" + subId + "/families"));
        operation.getChildren().getChildren().add(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_INSTANCES, baseApi + "/operations/operation" + subId + "/instances"));
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
        family.setChildren(new Children());
        family.getChildren().setTotal(BigInteger.valueOf(1));
        family.getChildren().getChildren().add(MetamacRestMocks.mockResourceLink(RestInternalConstants.KIND_OPERATIONS, baseApi + "/families/family" + subId + "/operations"));
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
        instance.setStatisticalUnits(new StatisticalUnits());
        instance.getStatisticalUnits().setTotal(BigInteger.valueOf(2));
        instance.getStatisticalUnits().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getStatisticalUnits().getStatisticalUnits().add(mockResourceFromExternalItemSrm("statisticalUnit1", "statisticalUnits", TypeExternalArtefactsEnum.CONCEPT));
        instance.getStatisticalUnits().getStatisticalUnits().add(mockResourceFromExternalItemSrm("statisticalUnit22", "statisticalUnits", TypeExternalArtefactsEnum.CONCEPT));
        instance.setGeographicGranularity(mockResourceFromExternalItemSrm("geographicGranularity", "geographicGranularities", TypeExternalArtefactsEnum.CODELIST));
        instance.setGeographicComparability(mockInternationalString("geographicComparability", subId));
        instance.setTemporalGranularity(mockResourceFromExternalItemSrm("temporalGranularity", "temporalGranularities", TypeExternalArtefactsEnum.CODELIST));
        instance.setTemporalComparability(mockInternationalString("temporalComparability", subId));
        instance.setBasePeriod("2012");
        instance.setUnitMeasures(new UnitMeasures());
        instance.getUnitMeasures().setTotal(BigInteger.valueOf(1));
        instance.getUnitMeasures().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getUnitMeasures().getUnitMeasures().add(mockResourceFromExternalItemSrm("unitMeasure1", "unitMeasures", TypeExternalArtefactsEnum.CONCEPT));
        instance.setStatConcDefsDescription(mockInternationalString("statConcDef", subId));
        instance.setStatConcDefs(new StatConcDefs());
        instance.getStatConcDefs().setTotal(BigInteger.valueOf(3));
        instance.getStatConcDefs().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getStatConcDefs().getStatConcDefs().add(mockResourceFromExternalItemSrm("statConcDefList1", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.getStatConcDefs().getStatConcDefs().add(mockResourceFromExternalItemSrm("statConcDefList22", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.getStatConcDefs().getStatConcDefs().add(mockResourceFromExternalItemSrm("statConcDefList333", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.setClassSystemsDescription(mockInternationalString("classSystem", subId));
        instance.setClassSystems(new ClassSystems());
        instance.getClassSystems().setTotal(BigInteger.valueOf(2));
        instance.getClassSystems().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getClassSystems().getClassSystems().add(mockResourceFromExternalItemSrm("classSystemList1", "classSystemLists", TypeExternalArtefactsEnum.CODELIST));
        instance.getClassSystems().getClassSystems().add(mockResourceFromExternalItemSrm("classSystemList22", "classSystemLists", TypeExternalArtefactsEnum.CODELIST));
        instance.setInstanceType(MetamacRestMocks.mockItem("instanceType1"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        instance.setProcStatus(procStatus);
        instance.setDocMethod(mockInternationalString("docMethod", subId));
        instance.setSurveySource(MetamacRestMocks.mockItem("surveySource1"));
        instance.setCollMethod(MetamacRestMocks.mockItem("collMethod1"));
        instance.setInformationSuppliers(new InformationSuppliers());
        instance.getInformationSuppliers().setTotal(BigInteger.valueOf(1));
        instance.getInformationSuppliers().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getInformationSuppliers().getInformationSuppliers().add(mockResourceFromExternalItemSrm("informationSupplier1", "informationSuppliers", TypeExternalArtefactsEnum.COMMON_METADATA));
        instance.setFreqColls(new FreqColls());
        instance.getFreqColls().setTotal(BigInteger.valueOf(2));
        instance.getFreqColls().setKind(Do2RestInternalMapperV10Impl.KIND_SRM_EXTERNAL_ITEM);
        instance.getFreqColls().getFreqColls().add(mockResourceFromExternalItemSrm("freqColl1", "freqColls", TypeExternalArtefactsEnum.CATEGORY_SCHEME));
        instance.getFreqColls().getFreqColls().add(mockResourceFromExternalItemSrm("freqColl22", "freqColls", TypeExternalArtefactsEnum.CATEGORY_SCHEME));
        instance.setDataValidation(mockInternationalString("dataValidation", subId));
        instance.setDataCompilation(mockInternationalString("dataCompilation", subId));
        instance.setAdjustment(mockInternationalString("adjustment", subId));
        instance.setCostBurden(mockInternationalString("costBurden", subId));
        instance.setCosts(new Costs());
        instance.getCosts().setTotal(BigInteger.valueOf(4));
        instance.getCosts().setKind(RestInternalConstants.KIND_COSTS);
        instance.getCosts().getCosts().add(MetamacRestMocks.mockItem("cost1"));
        instance.getCosts().getCosts().add(MetamacRestMocks.mockItem("cost22"));
        instance.getCosts().getCosts().add(MetamacRestMocks.mockItem("cost333"));
        instance.getCosts().getCosts().add(MetamacRestMocks.mockItem("cost4444"));
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
        // no children
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